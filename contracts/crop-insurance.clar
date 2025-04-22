
;; title: crop-insurance

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-registered (err u101))
(define-constant err-already-registered (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-invalid-region (err u104))
(define-constant err-invalid-crop (err u105))
(define-constant err-invalid-amount (err u106))
(define-constant err-no-policy (err u107))
(define-constant err-already-claimed (err u108))
(define-constant err-not-claimable (err u109))

(define-data-var min-premium-amount uint u1000000)
(define-data-var oracle-address principal 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
(define-data-var total-stx-pool uint u0)

(define-map regions uint {
  name: (string-ascii 20),
  active: bool
})

(define-map crops uint {
  name: (string-ascii 20),
  active: bool
})

(define-map farmers principal {
  registered: bool,
  total-premiums-paid: uint,
  total-claims-received: uint
})

(define-map policies 
  { farmer: principal, policy-id: uint } 
  {
    region-id: uint,
    crop-id: uint,
    premium-amount: uint,
    coverage-amount: uint,
    start-block: uint,
    end-block: uint,
    claimed: bool
  }
)

(define-map weather-data 
  { region-id: uint, stacks-block-height: uint } 
  {
    rainfall: uint,
    temperature: uint,
    drought-index: uint,
    timestamp: uint
  }
)

(define-map policy-counter principal uint)

(define-map claims 
  { farmer: principal, policy-id: uint } 
  {
    amount: uint,
    stacks-block-height: uint,
    weather-data-ref: { region-id: uint, stacks-block-height: uint }
  }
)

(define-read-only (get-farmer-info (farmer principal))
  (default-to 
    { registered: false, total-premiums-paid: u0, total-claims-received: u0 }
    (map-get? farmers farmer)
  )
)

(define-read-only (get-policy (farmer principal) (policy-id uint))
  (map-get? policies { farmer: farmer, policy-id: policy-id })
)

(define-read-only (get-weather-data (region-id uint) (stacks-block-heights uint))
  (map-get? weather-data { region-id: region-id, stacks-block-height: stacks-block-height })
)

(define-read-only (get-region (region-id uint))
  (map-get? regions region-id)
)

(define-read-only (get-crop (crop-id uint))
  (map-get? crops crop-id)
)

(define-read-only (get-claim (farmer principal) (policy-id uint))
  (map-get? claims { farmer: farmer, policy-id: policy-id })
)

(define-read-only (get-next-policy-id (farmer principal))
  (default-to u1 (map-get? policy-counter farmer))
)

(define-public (register-farmer)
  (let ((farmer-info (get-farmer-info tx-sender)))
    (if (get registered farmer-info)
      err-already-registered
      (begin
        (map-set farmers tx-sender {
          registered: true,
          total-premiums-paid: u0,
          total-claims-received: u0
        })
        (ok true)
      )
    )
  )
)

(define-public (add-region (region-id uint) (name (string-ascii 20)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set regions region-id {
      name: name,
      active: true
    })
    (ok true)
  )
)

(define-public (add-crop (crop-id uint) (name (string-ascii 20)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set crops crop-id {
      name: name,
      active: true
    })
    (ok true)
  )
)

(define-public (purchase-insurance (region-id uint) (crop-id uint) (premium-amount uint) (coverage-amount uint) (duration uint))
  (let (
    (farmer-info (get-farmer-info tx-sender))
    (region (get-region region-id))
    (crop (get-crop crop-id))
    (policy-id (get-next-policy-id tx-sender))
    (current-block stacks-block-height)
  )
    (asserts! (get registered farmer-info) err-not-registered)
    (asserts! (is-some region) err-invalid-region)
    (asserts! (is-some crop) err-invalid-crop)
    (asserts! (>= premium-amount (var-get min-premium-amount)) err-invalid-amount)
    (asserts! (> coverage-amount premium-amount) err-invalid-amount)
    
    (try! (stx-transfer? premium-amount tx-sender (as-contract tx-sender)))
    
    (var-set total-stx-pool (+ (var-get total-stx-pool) premium-amount))
    
    (map-set policies 
      { farmer: tx-sender, policy-id: policy-id } 
      {
        region-id: region-id,
        crop-id: crop-id,
        premium-amount: premium-amount,
        coverage-amount: coverage-amount,
        start-block: current-block,
        end-block: (+ current-block duration),
        claimed: false
      }
    )
    
    (map-set policy-counter tx-sender (+ policy-id u1))
    
    (map-set farmers tx-sender {
      registered: true,
      total-premiums-paid: (+ (get total-premiums-paid farmer-info) premium-amount),
      total-claims-received: (get total-claims-received farmer-info)
    })
    
    (ok policy-id)
  )
)

(define-public (submit-weather-data (region-id uint) (rainfall uint) (temperature uint) (drought-index uint))
  (begin
    (asserts! (is-eq tx-sender (var-get oracle-address)) err-owner-only)
    (asserts! (is-some (get-region region-id)) err-invalid-region)
    
    (map-set weather-data 
      { region-id: region-id, stacks-block-height: stacks-block-height } 
      {
        rainfall: rainfall,
        temperature: temperature,
        drought-index: drought-index,
        timestamp: (unwrap-panic (get-stacks-block-info? time stacks-block-height))
      }
    )
    
    (ok true)
  )
)

(define-public (claim-insurance (policy-id uint))
  (let (
    (farmer tx-sender)
    (policy (get-policy farmer policy-id))
    (farmer-info (get-farmer-info farmer))
  )
    (asserts! (is-some policy) err-no-policy)
    (let (
      (policy-data (unwrap-panic policy))
      (weather-data-entry (get-weather-data (get region-id policy-data) stacks-block-height))
    )
      (asserts! (not (get claimed policy-data)) err-already-claimed)
      (asserts! (<= stacks-block-height (get end-block policy-data)) err-not-claimable)
      (asserts! (is-some weather-data-entry) err-not-claimable)
      
      (let (
        (weather-info (unwrap-panic weather-data-entry))
        (payout-amount (calculate-payout 
                         (get drought-index weather-info) 
                         (get coverage-amount policy-data)))
      )
        (asserts! (> payout-amount u0) err-not-claimable)
        (asserts! (<= payout-amount (var-get total-stx-pool)) err-insufficient-funds)
        
        (try! (as-contract (stx-transfer? payout-amount (as-contract tx-sender) farmer)))
        
        (var-set total-stx-pool (- (var-get total-stx-pool) payout-amount))
        
        (map-set policies 
          { farmer: farmer, policy-id: policy-id } 
          (merge policy-data { claimed: true })
        )
        
        (map-set claims 
          { farmer: farmer, policy-id: policy-id } 
          {
            amount: payout-amount,
            stacks-block-height: stacks-block-height,
            weather-data-ref: { region-id: (get region-id policy-data), stacks-block-height: stacks-block-height }
          }
        )
        
        (map-set farmers farmer {
          registered: true,
          total-premiums-paid: (get total-premiums-paid farmer-info),
          total-claims-received: (+ (get total-claims-received farmer-info) payout-amount)
        })
        
        (ok payout-amount)
      )
    )
  )
)

(define-public (set-oracle-address (new-oracle principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set oracle-address new-oracle)
    (ok true)
  )
)

(define-public (set-min-premium (new-min-premium uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set min-premium-amount new-min-premium)
    (ok true)
  )
)

(define-read-only (calculate-payout (drought-index uint) (coverage-amount uint))
  (if (> drought-index u70)
    coverage-amount
    (if (> drought-index u50)
      (/ (* coverage-amount u75) u100)
      (if (> drought-index u30)
        (/ (* coverage-amount u50) u100)
        u0
      )
    )
  )
)
