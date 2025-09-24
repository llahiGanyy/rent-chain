;; ------------------------------------------------
;; Contract: rent-chain
;; Trustless Rental Agreement Smart Contract
;; ------------------------------------------------

;; Error codes
(define-constant ERR_NOT_LANDLORD (err u100))
(define-constant ERR_NOT_TENANT (err u101))
(define-constant ERR_ALREADY_ACTIVE (err u102))
(define-constant ERR_NO_ACTIVE_AGREEMENT (err u103))
(define-constant ERR_TOO_EARLY (err u104))
(define-constant ERR_ALREADY_TERMINATED (err u105))
(define-constant ERR_PAYMENT_DUE (err u106))
(define-constant ERR_INVALID_AMOUNT (err u107))
(define-constant ERR_INVALID_INTERVAL (err u108))
(define-constant ERR_INVALID_DURATION (err u109))
(define-constant ERR_INVALID_TENANT (err u110))

;; Track rental agreements
(define-map rentals
  { id: uint }
  { landlord: principal,
    tenant: principal,
    rent-amount: uint,
    start-block: uint,
    interval: uint,
    duration: uint,
    paid-periods: uint,
    terminated: bool })

(define-data-var rental-counter uint u0)

;; Create rental agreement
(define-public (create-rental (tenant principal) (rent-amount uint) (interval uint) (duration uint))
  (let ((check-amount (> rent-amount u0))
        (check-interval (> interval u0))
        (check-duration (> duration u0))
        (check-tenant (not (is-eq tenant tx-sender))))
    (if check-amount
      (if check-interval
        (if check-duration
          (if check-tenant
            (let ((new-id (+ (var-get rental-counter) u1)))
              (var-set rental-counter new-id)
              (ok (map-set rentals 
                { id: new-id }
                { landlord: tx-sender,
                  tenant: tenant,
                  rent-amount: rent-amount,
                  start-block: stacks-block-height,
                  interval: interval,
                  duration: duration,
                  paid-periods: u0,
                  terminated: false })))
            ERR_INVALID_TENANT)
          ERR_INVALID_DURATION)
        ERR_INVALID_INTERVAL)
      ERR_INVALID_AMOUNT)))

;; Pay rent for current period
(define-public (pay-rent (rental-id uint))
  (if (<= rental-id (var-get rental-counter))
    (let ((rental (map-get? rentals { id: rental-id })))
      (match rental
        rental-data
          (let ((is-tenant (is-eq tx-sender (get tenant rental-data)))
                (next-payment-block (+ (get start-block rental-data) 
                                     (* (get interval rental-data) 
                                        (get paid-periods rental-data)))))
            (if is-tenant
              (if (not (get terminated rental-data))
                (if (< (get paid-periods rental-data) (get duration rental-data))
                  (if (>= stacks-block-height next-payment-block)
                    (begin
                      (try! (stx-transfer? (get rent-amount rental-data) 
                                         (get landlord rental-data) 
                                         tx-sender))
                      (ok (map-set rentals 
                        { id: rental-id }
                        { landlord: (get landlord rental-data),
                          tenant: (get tenant rental-data),
                          rent-amount: (get rent-amount rental-data),
                          start-block: (get start-block rental-data),
                          interval: (get interval rental-data),
                          duration: (get duration rental-data),
                          paid-periods: (+ (get paid-periods rental-data) u1),
                          terminated: false })))
                    ERR_TOO_EARLY)
                  ERR_ALREADY_TERMINATED)
                ERR_ALREADY_TERMINATED)
              ERR_NOT_TENANT))
        ERR_NO_ACTIVE_AGREEMENT))
    ERR_NO_ACTIVE_AGREEMENT))

;; Landlord terminates if tenant misses payment
(define-public (terminate (rental-id uint))
  (if (<= rental-id (var-get rental-counter))
    (let ((rental (map-get? rentals { id: rental-id })))
      (match rental
        rental-data
          (let ((is-landlord (is-eq tx-sender (get landlord rental-data)))
                (next-payment-block (+ (get start-block rental-data) 
                                     (* (get interval rental-data) 
                                        (get paid-periods rental-data)))))
            (if is-landlord
              (if (not (get terminated rental-data))
                (if (< (get paid-periods rental-data) (get duration rental-data))
                  (if (>= stacks-block-height next-payment-block)
                    (ok (map-set rentals
                      { id: rental-id }
                      { landlord: (get landlord rental-data),
                        tenant: (get tenant rental-data),
                        rent-amount: (get rent-amount rental-data),
                        start-block: (get start-block rental-data),
                        interval: (get interval rental-data),
                        duration: (get duration rental-data),
                        paid-periods: (get paid-periods rental-data),
                        terminated: true }))
                    ERR_PAYMENT_DUE)
                  ERR_ALREADY_TERMINATED)
                ERR_ALREADY_TERMINATED)
              ERR_NOT_LANDLORD))
        ERR_NO_ACTIVE_AGREEMENT))
    ERR_NO_ACTIVE_AGREEMENT))

;; View rental agreement
(define-read-only (get-rental (rental-id uint))
  (map-get? rentals { id: rental-id }))

;; Check if rental completed
(define-read-only (is-completed (rental-id uint))
  (let ((rental (map-get? rentals { id: rental-id })))
    (match rental
      rental-data 
        (ok (if (>= (get paid-periods rental-data) (get duration rental-data))
              (not (get terminated rental-data))
              false))
      ERR_NO_ACTIVE_AGREEMENT)))
