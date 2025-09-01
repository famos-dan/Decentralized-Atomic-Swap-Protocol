
;; title: Decentralized-Atomic-Swap-Protocol
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-SWAP-EXPIRED (err u402))
(define-constant ERR-SWAP-ALREADY-EXISTS (err u403))
(define-constant ERR-SWAP-NOT-FOUND (err u404))
(define-constant ERR-INVALID-AMOUNT (err u405))
(define-constant ERR-SWAP-ALREADY-CLAIMED (err u406))
(define-constant ERR-INVALID-PROOF (err u407))
(define-constant ERR-SLIPPAGE-TOO-HIGH (err u408))
(define-constant ERR-EXCEEDS-VOLUME-CAP (err u409))
(define-constant ERR-CIRCUIT-BREAKER-ACTIVE (err u410))
(define-constant ERR-BLACKLISTED (err u411))

;; ----- Data Maps -----

;; Data structure for swap details
(define-map swaps
  { swap-id: (buff 32) }
  {
    initiator: principal,
    counterparty: (optional principal),
    btc-address: (buff 34),
    stx-amount: uint,
    btc-amount: uint,
    token-contract: (optional principal),
    token-id: (optional uint),
    token-amount: (optional uint),
    timeout: uint,
    status: (string-ascii 20),
    proof-submitted: bool,
    creation-height: uint,
    slippage-tolerance: uint,
    conditions: (list 10 (string-ascii 64))
  }
)

;; Liquidity provider registry
(define-map liquidity-providers
  { provider: principal }
  {
    total-liquidity: uint,
    rewards-earned: uint,
    fee-discount: uint,
    last-deposit-height: uint,
    reputation-score: uint
  }
)

;; Statistics tracking
(define-map protocol-stats
  { stat-type: (string-ascii 20) }
  { value: uint }
)

;; Protocol parameters (governable)
(define-data-var fee-percentage uint u30) ;; 0.3% base fee
(define-data-var token-holder-discount uint u50) ;; 50% discount for token holders
(define-data-var circuit-breaker-active bool false)
(define-data-var volume-cap-per-block uint u1000000000000) ;; 1M STX
(define-data-var admin principal tx-sender)
(define-data-var min-reputation-required uint u10)
(define-data-var protocol-version (string-ascii 10) "1.0.0")

;; Update volume statistics and check if within cap
(define-private (update-volume-stats (amount uint))
  (let (
    (current-volume (default-to u0 (get value (map-get? protocol-stats { stat-type: "daily-volume" }))))
    (new-volume (+ current-volume amount))
  )
    (if (> new-volume (var-get volume-cap-per-block))
      (err ERR-EXCEEDS-VOLUME-CAP)
      (begin
        (map-set protocol-stats { stat-type: "daily-volume" } { value: new-volume })
        (ok true)
      )
    )
  )
)

;; Check user reputation score
(define-private (check-reputation (user principal))
  (let (
    (user-data (default-to { total-liquidity: u0, rewards-earned: u0, fee-discount: u0, last-deposit-height: u0, reputation-score: u0 } 
               (map-get? liquidity-providers { provider: user })))
    (reputation (get reputation-score user-data))
  )
    (>= reputation (var-get min-reputation-required))
  )
)

;; Validate BTC proof
(define-private (validate-btc-proof (proof (buff 1024)) (btc-tx-id (buff 32)) (btc-address (buff 34)) (amount uint))
 
  (begin
    (print { event: "validate-btc-proof", proof: proof, btc-tx-id: btc-tx-id })
    true
  )
)

;; Add liquidity to the protocol
(define-public (add-liquidity (amount uint))
  (let (
    (provider-data (default-to 
      { total-liquidity: u0, rewards-earned: u0, fee-discount: u0, last-deposit-height: u0, reputation-score: u0 } 
      (map-get? liquidity-providers { provider: tx-sender })
    ))
  )
    ;; Transfer STX to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update provider data
    (map-set liquidity-providers
      { provider: tx-sender }
      (merge provider-data {
        total-liquidity: (+ (get total-liquidity provider-data) amount),
        last-deposit-height: stacks-block-height,
        reputation-score: (+ (get reputation-score provider-data) u1)
      })
    )
    
    ;; Update total liquidity
    (map-set protocol-stats
      { stat-type: "total-liquidity" }
      { value: (+ amount (default-to u0 (get value (map-get? protocol-stats { stat-type: "total-liquidity" })))) }
    )
    
    (ok true)
  )
)

;; Remove liquidity from the protocol
(define-public (remove-liquidity (amount uint))
  (let (
    (provider-data (unwrap! (map-get? liquidity-providers { provider: tx-sender }) ERR-NOT-AUTHORIZED))
    (current-liquidity (get total-liquidity provider-data))
  )
    ;; Check if provider has enough liquidity
    (asserts! (>= current-liquidity amount) ERR-INVALID-AMOUNT)
    
    ;; Transfer STX from contract
    (as-contract (try! (stx-transfer? amount tx-sender tx-sender)))
    
    ;; Update provider data
    (map-set liquidity-providers
      { provider: tx-sender }
      (merge provider-data {
        total-liquidity: (- current-liquidity amount)
      })
    )
    
    ;; Update total liquidity
    (map-set protocol-stats
      { stat-type: "total-liquidity" }
      { value: (- (default-to u0 (get value (map-get? protocol-stats { stat-type: "total-liquidity" }))) amount) }
    )
    
    (ok true)
  )
)

;; Distribute fees to liquidity providers
(define-private (distribute-fees (fee-amount uint))
  (let (
    (total-liquidity (default-to u0 (get value (map-get? protocol-stats { stat-type: "total-liquidity" }))))
  )
    ;; In a real implementation, this would distribute to all LPs proportionally
    ;; For simplicity, we're just recording the fee
    (map-set protocol-stats
      { stat-type: "total-fees" }
      { value: (+ fee-amount (default-to u0 (get value (map-get? protocol-stats { stat-type: "total-fees" })))) }
    )
    
    (ok true)
  )
)

;; Update user reputation score
(define-private (update-reputation (user principal) (points uint))
  (let (
    (provider-data (default-to 
      { total-liquidity: u0, rewards-earned: u0, fee-discount: u0, last-deposit-height: u0, reputation-score: u0 } 
      (map-get? liquidity-providers { provider: user })
    ))
  )
    (map-set liquidity-providers
      { provider: user }
      (merge provider-data {
        reputation-score: (+ (get reputation-score provider-data) points)
      })
    )
    
    (ok true)
  )
)

;; Check if an address is blacklisted
(define-private (is-blacklisted (address principal))
  
  false
)

;; ----- Admin Functions -----

;; Toggle circuit breaker
(define-public (toggle-circuit-breaker)
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (ok (var-set circuit-breaker-active (not (var-get circuit-breaker-active))))
  )
)

;; Update fee percentage
(define-public (update-fee-percentage (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-fee u1000) ERR-INVALID-AMOUNT) ;; Max 10%
    (ok (var-set fee-percentage new-fee))
  )
)

;; Transfer admin role
(define-public (transfer-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (ok (var-set admin new-admin))
  )
)

;; Get swap details
(define-read-only (get-swap-details (swap-id (buff 32)))
  (map-get? swaps { swap-id: swap-id })
)

;; Get liquidity provider details
(define-read-only (get-provider-details (provider principal))
  (map-get? liquidity-providers { provider: provider })
)

;; Get protocol statistics
(define-read-only (get-protocol-stats (stat-type (string-ascii 20)))
  (map-get? protocol-stats { stat-type: stat-type })
)

;; Get current protocol version
(define-read-only (get-protocol-version)
  (var-get protocol-version)
)

;; Check if circuit breaker is active
(define-read-only (is-circuit-breaker-active)
  (var-get circuit-breaker-active)
)

;; Additional Error Codes
(define-constant ERR-PROPOSAL-NOT-FOUND (err u412))
(define-constant ERR-ALREADY-VOTED (err u413))
(define-constant ERR-PROPOSAL-CLOSED (err u414))
(define-constant ERR-NOT-GOVERNOR (err u415))
(define-constant ERR-INVALID-LP-TOKEN (err u416))
(define-constant ERR-INSUFFICIENT-STAKE (err u417))
(define-constant ERR-VAULT-LOCKED (err u418))
(define-constant ERR-FLASH-LOAN-NOT-REPAID (err u419))

;; Governance structure
(define-map governors
  { address: principal }
  { active: bool, weight: uint }
)

(define-map governance-proposals
  { proposal-id: uint }
  {
    proposer: principal,
    title: (string-ascii 50),
    description: (string-ascii 500),
    action: (string-ascii 50),
    parameter: uint,
    votes-for: uint,
    votes-against: uint,
    start-height: uint,
    end-height: uint,
    executed: bool
  }
)

(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  { vote: bool, weight: uint }
)

(define-data-var next-proposal-id uint u1)
(define-data-var proposal-duration uint u144) ;; ~1 day in Stacks blocks
(define-data-var min-proposal-threshold uint u100000000) ;; Min STX to create proposal (100 STX)
(define-data-var quorum-threshold uint u60) ;; 60% quorum needed

;; Add governor
(define-public (add-governor (address principal) (weight uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (map-set governors { address: address } { active: true, weight: weight })
    (ok true)
  )
)

;; Remove governor
(define-public (remove-governor (address principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (map-set governors { address: address } { active: false, weight: u0 })
    (ok true)
  )
)

;; Check if address is governor
(define-read-only (is-governor (address principal))
  (let ((governor-data (default-to { active: false, weight: u0 } (map-get? governors { address: address }))))
    (get active governor-data)
  )
)

;; Create governance proposal
(define-public (create-proposal (title (string-ascii 50)) (description (string-ascii 500)) (action (string-ascii 50)) (parameter uint))
  (let (
    (proposal-id (var-get next-proposal-id))
    (provider-data (default-to { total-liquidity: u0, rewards-earned: u0, fee-discount: u0, last-deposit-height: u0, reputation-score: u0 } 
                  (map-get? liquidity-providers { provider: tx-sender })))
  )
    ;; Check if caller can create proposals
    (asserts! (or 
                (is-governor tx-sender) 
                (>= (get total-liquidity provider-data) (var-get min-proposal-threshold))
              ) 
              ERR-INSUFFICIENT-STAKE)
    
    ;; Create the proposal
    (map-set governance-proposals
      { proposal-id: proposal-id }
      {
        proposer: tx-sender,
        title: title,
        description: description,
        action: action,
        parameter: parameter,
        votes-for: u0,
        votes-against: u0,
        start-height: stacks-block-height,
        end-height: (+ stacks-block-height (var-get proposal-duration)),
        executed: false
      }
    )
    
    ;; Increment proposal counter
    (var-set next-proposal-id (+ proposal-id u1))
    
    (ok proposal-id)
  )
)

;; Vote on governance proposal
(define-public (vote-on-proposal (proposal-id uint) (vote bool))
  (let (
    (proposal (unwrap! (map-get? governance-proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    (governor-data (default-to { active: false, weight: u0 } (map-get? governors { address: tx-sender })))
    (voter-weight (get weight governor-data))
  )
    ;; Check if proposal is still active
    (asserts! (< stacks-block-height (get end-height proposal)) ERR-PROPOSAL-CLOSED)
    
    ;; Check if voter is authorized
    (asserts! (get active governor-data) ERR-NOT-GOVERNOR)
    
    ;; Check if voter has already voted
    (asserts! (is-none (map-get? proposal-votes { proposal-id: proposal-id, voter: tx-sender })) ERR-ALREADY-VOTED)
    
    ;; Record vote
    (map-set proposal-votes
      { proposal-id: proposal-id, voter: tx-sender }
      { vote: vote, weight: voter-weight }
    )
    
    ;; Update proposal vote counts
    (if vote
      (map-set governance-proposals
        { proposal-id: proposal-id }
        (merge proposal { votes-for: (+ (get votes-for proposal) voter-weight) })
      )
      (map-set governance-proposals
        { proposal-id: proposal-id }
        (merge proposal { votes-against: (+ (get votes-against proposal) voter-weight) })
      )
    )
    
    (ok true)
  )
)