;; Emergency Relief Fund Contract
;; Provides immediate disaster relief for farmers during catastrophic events

;; Error constants
(define-constant err-unauthorized (err u300))
(define-constant err-fund-not-found (err u301))
(define-constant err-insufficient-funds (err u302))
(define-constant err-invalid-amount (err u303))
(define-constant err-already-declared (err u304))
(define-constant err-no-emergency (err u305))
(define-constant err-already-received (err u306))
(define-constant err-not-eligible (err u307))
(define-constant err-emergency-expired (err u308))

;; Contract owner
(define-constant contract-owner tx-sender)

;; Configuration variables
(define-data-var min-donation-amount uint u100000) ;; 0.1 STX minimum donation
(define-data-var emergency-duration-blocks uint u1440) ;; ~10 days
(define-data-var max-relief-percentage uint u30) ;; Max 30% of fund per emergency
(define-data-var fund-counter uint u0)
(define-data-var emergency-counter uint u0)

;; Emergency relief funds by region/disaster type
(define-map relief-funds
  uint
  {
    name: (string-ascii 50),
    region-id: uint,
    disaster-type: (string-ascii 20),
    total-donations: uint,
    total-distributed: uint,
    active: bool,
    created-by: principal,
    created-block: uint
  }
)

;; Individual donations tracking
(define-map donations
  { fund-id: uint, donor: principal }
  {
    amount: uint,
    donation-block: uint
  }
)

;; Emergency declarations by authorized officials
(define-map emergency-declarations
  uint
  {
    region-id: uint,
    disaster-type: (string-ascii 20),
    severity-level: uint, ;; 1-5 scale
    declared-by: principal,
    declared-block: uint,
    expires-block: uint,
    affected-farmers: uint,
    active: bool
  }
)

;; Relief distributions to farmers
(define-map relief-distributions
  { emergency-id: uint, farmer: principal }
  {
    amount: uint,
    fund-id: uint,
    distributed-block: uint
  }
)

;; Authorized emergency officials
(define-map authorized-officials principal bool)

;; Fund donation summary per donor
(define-map donor-summary
  principal
  {
    total-donated: uint,
    funds-supported: uint,
    first-donation-block: uint
  }
)

;; Read-only functions
(define-read-only (get-relief-fund (fund-id uint))
  (map-get? relief-funds fund-id)
)

(define-read-only (get-donation (fund-id uint) (donor principal))
  (map-get? donations { fund-id: fund-id, donor: donor })
)

(define-read-only (get-emergency-declaration (emergency-id uint))
  (map-get? emergency-declarations emergency-id)
)

(define-read-only (get-relief-distribution (emergency-id uint) (farmer principal))
  (map-get? relief-distributions { emergency-id: emergency-id, farmer: farmer })
)

(define-read-only (get-donor-summary (donor principal))
  (default-to
    { total-donated: u0, funds-supported: u0, first-donation-block: u0 }
    (map-get? donor-summary donor)
  )
)

(define-read-only (is-authorized-official (official principal))
  (default-to false (map-get? authorized-officials official))
)

(define-read-only (get-next-fund-id)
  (+ (var-get fund-counter) u1)
)

(define-read-only (calculate-relief-amount (fund-balance uint) (severity-level uint) (affected-farmers uint))
  (let (
    (base-amount (/ fund-balance u10)) ;; 10% base amount
    (severity-multiplier (+ u50 (* severity-level u10))) ;; 60% to 100% based on severity
    (distribution-per-farmer (/ (* base-amount severity-multiplier) (* affected-farmers u100)))
  )
    (if (> distribution-per-farmer u0) distribution-per-farmer u50000) ;; Minimum 0.05 STX per farmer
  )
)

;; Administrative functions
(define-public (authorize-official (official principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (map-set authorized-officials official true)
    (ok true)
  )
)

(define-public (revoke-official (official principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (map-set authorized-officials official false)
    (ok true)
  )
)

(define-public (update-min-donation (new-amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (var-set min-donation-amount new-amount)
    (ok true)
  )
)

(define-public (update-emergency-duration (new-duration uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (var-set emergency-duration-blocks new-duration)
    (ok true)
  )
)

;; Fund management functions
(define-public (create-relief-fund (name (string-ascii 50)) (region-id uint) (disaster-type (string-ascii 20)))
  (let (
    (fund-id (get-next-fund-id))
  )
    (map-set relief-funds fund-id {
      name: name,
      region-id: region-id,
      disaster-type: disaster-type,
      total-donations: u0,
      total-distributed: u0,
      active: true,
      created-by: tx-sender,
      created-block: stacks-block-height
    })
    (var-set fund-counter fund-id)
    (ok fund-id)
  )
)

(define-public (donate-to-fund (fund-id uint) (amount uint))
  (let (
    (fund-info (unwrap! (get-relief-fund fund-id) err-fund-not-found))
    (existing-donation (get-donation fund-id tx-sender))
    (donor-info (get-donor-summary tx-sender))
  )
    (asserts! (get active fund-info) err-fund-not-found)
    (asserts! (>= amount (var-get min-donation-amount)) err-invalid-amount)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update fund total
    (map-set relief-funds fund-id
      (merge fund-info { total-donations: (+ (get total-donations fund-info) amount) })
    )
    
    ;; Update or create donation record
    (map-set donations { fund-id: fund-id, donor: tx-sender }
      {
        amount: (+ (default-to u0 (get amount existing-donation)) amount),
        donation-block: stacks-block-height
      }
    )
    
    ;; Update donor summary
    (map-set donor-summary tx-sender {
      total-donated: (+ (get total-donated donor-info) amount),
      funds-supported: (if (is-none existing-donation) (+ (get funds-supported donor-info) u1) (get funds-supported donor-info)),
      first-donation-block: (if (is-eq (get first-donation-block donor-info) u0) stacks-block-height (get first-donation-block donor-info))
    })
    
    (ok true)
  )
)

;; Emergency declaration and relief distribution
(define-public (declare-emergency (region-id uint) (disaster-type (string-ascii 20)) (severity-level uint) (affected-farmers uint))
  (let (
    (emergency-id (+ (var-get emergency-counter) u1))
  )
    (asserts! (is-authorized-official tx-sender) err-unauthorized)
    (asserts! (and (>= severity-level u1) (<= severity-level u5)) err-invalid-amount)
    (asserts! (> affected-farmers u0) err-invalid-amount)
    
    (map-set emergency-declarations emergency-id {
      region-id: region-id,
      disaster-type: disaster-type,
      severity-level: severity-level,
      declared-by: tx-sender,
      declared-block: stacks-block-height,
      expires-block: (+ stacks-block-height (var-get emergency-duration-blocks)),
      affected-farmers: affected-farmers,
      active: true
    })
    
    (var-set emergency-counter emergency-id)
    (ok emergency-id)
  )
)

(define-public (request-emergency-relief (emergency-id uint) (fund-id uint))
  (let (
    (emergency-info (unwrap! (get-emergency-declaration emergency-id) err-no-emergency))
    (fund-info (unwrap! (get-relief-fund fund-id) err-fund-not-found))
    (existing-relief (get-relief-distribution emergency-id tx-sender))
  )
    (asserts! (get active emergency-info) err-no-emergency)
    (asserts! (get active fund-info) err-fund-not-found)
    (asserts! (< stacks-block-height (get expires-block emergency-info)) err-emergency-expired)
    (asserts! (is-none existing-relief) err-already-received)
    (asserts! (is-eq (get region-id emergency-info) (get region-id fund-info)) err-not-eligible)
    (asserts! (is-eq (get disaster-type emergency-info) (get disaster-type fund-info)) err-not-eligible)
    
    (let (
      (relief-amount (calculate-relief-amount 
                       (get total-donations fund-info) 
                       (get severity-level emergency-info) 
                       (get affected-farmers emergency-info)))
      (available-funds (- (get total-donations fund-info) (get total-distributed fund-info)))
    )
      (asserts! (<= relief-amount available-funds) err-insufficient-funds)
      
      ;; Transfer relief payment
      (try! (as-contract (stx-transfer? relief-amount (as-contract tx-sender) tx-sender)))
      
      ;; Record distribution
      (map-set relief-distributions { emergency-id: emergency-id, farmer: tx-sender }
        {
          amount: relief-amount,
          fund-id: fund-id,
          distributed-block: stacks-block-height
        }
      )
      
      ;; Update fund distributed total
      (map-set relief-funds fund-id
        (merge fund-info { total-distributed: (+ (get total-distributed fund-info) relief-amount) })
      )
      
      (ok relief-amount)
    )
  )
)

(define-public (close-emergency (emergency-id uint))
  (let (
    (emergency-info (unwrap! (get-emergency-declaration emergency-id) err-no-emergency))
  )
    (asserts! (is-authorized-official tx-sender) err-unauthorized)
    (asserts! (get active emergency-info) err-no-emergency)
    
    (map-set emergency-declarations emergency-id
      (merge emergency-info { active: false })
    )
    (ok true)
  )
)

(define-public (deactivate-fund (fund-id uint))
  (let (
    (fund-info (unwrap! (get-relief-fund fund-id) err-fund-not-found))
  )
    (asserts! (is-eq tx-sender (get created-by fund-info)) err-unauthorized)
    (asserts! (get active fund-info) err-fund-not-found)
    
    (map-set relief-funds fund-id
      (merge fund-info { active: false })
    )
    (ok true)
  )
)
