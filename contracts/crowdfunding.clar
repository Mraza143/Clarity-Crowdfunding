
(define-constant error-general (err u1))
(define-constant error-not-owner (err u2))
(define-constant error-campaign-has-investors (err u3))
(define-constant error-campaign-does-not-exist (err u4))
(define-constant error-campaign-inactive (err u5))
(define-constant error-invest-amount-insufficient (err u6))
(define-constant error-invest-stx-transfer-failed (err u7))
(define-constant error-no-investment (err u8))
(define-constant error-campaign-already-funded (err u9))
(define-constant error-refund-stx-transfer-failed (err u10))
(define-constant error-target-not-reached (err u11))
(define-constant error-funding-stx-transfer-failed (err u12))
(define-constant error-already-funded (err u13))


(define-data-var campaign-id-nonce uint u0)
(define-data-var total-campaigns-funded uint u0)
(define-data-var total-investments uint u0)
(define-data-var total-investment-value uint u0)



(define-map campaigns ((campaign-id uint))
	(
		(name (buff 64))			
		(fundraiser principal)		
		(goal uint)					
		(target-block-height uint)	
	))


(define-map campaign-information ((campaign-id uint))
	(
		(description (buff 280))	
		(link (buff 150))			
	))


(define-map campaign-totals ((campaign-id uint))
	(
		(total-investment uint)
		(total-investors uint)
	))


(define-map campaign-status ((campaign-id uint))
	(
		(target-reached bool)		
		(target-reached-height uint)
		(funded bool)				
	))


(define-map investments-principal ((campaign-id uint) (investor principal))
	(
		(amount uint)
	))




(define-read-only (get-campaign-id-nonce)
	(ok (var-get campaign-id-nonce))
	)


(define-read-only (get-total-campaigns-funded)
	(ok (var-get total-campaigns-funded))
	)


(define-read-only (get-total-investments)
	(ok (var-get total-investments))
	)


(define-read-only (get-total-investment-value)
	(ok (var-get total-investment-value))
	)


(define-read-only (get-campaign (campaign-id uint))
	(ok (map-get? campaigns ((campaign-id campaign-id))))
	)


(define-read-only (get-campaign-information (campaign-id uint))
	(ok (map-get? campaign-information ((campaign-id campaign-id))))
	)

(define-read-only (get-campaign-totals (campaign-id uint))
	(ok (map-get? campaign-totals ((campaign-id campaign-id))))
	)

(define-read-only (get-campaign-status (campaign-id uint))
	(ok (map-get? campaign-status ((campaign-id campaign-id))))
	)


(define-read-only (get-is-active-campaign (campaign-id uint))
	(let (
		(campaign (unwrap! (map-get? campaigns ((campaign-id campaign-id))) (ok false)))
		(status (unwrap! (map-get? campaign-status ((campaign-id campaign-id))) (ok false)))
		)
		(ok (and (< block-height (get target-block-height campaign)) (not (get target-reached status))))
		)
	)


(define-public (create-campaign (name (buff 64)) (description (buff 280)) (link (buff 150)) (goal uint) (duration uint))
	(let ((campaign-id (+ (var-get campaign-id-nonce) u1)))
		(if (and
				(map-set campaigns ((campaign-id campaign-id))
					(
						(name name)
						(fundraiser tx-sender)
						(goal goal)
						(target-block-height (+ duration block-height))
					))
				(map-set campaign-information ((campaign-id campaign-id))
					(
						(description description)
						(link link)
					))
				(map-set campaign-totals ((campaign-id campaign-id))
					(
						(total-investment u0)
						(total-investors u0)
					))
				(map-set campaign-status ((campaign-id campaign-id))
					(
						(target-reached false)
						(target-reached-height u0)
						(funded false)
					))
				)
			(begin
				(var-set campaign-id-nonce campaign-id)
				(ok campaign-id))
			error-general ;; else
			)
		)
	)

(define-public (update-campaign-information (campaign-id uint) (description (buff 280)) (link (buff 150)))
	(let ((campaign (unwrap! (map-get? campaigns ((campaign-id campaign-id))) error-campaign-does-not-exist)))
		(asserts! (is-eq (get fundraiser campaign) tx-sender) error-not-owner)
		(map-set campaign-information ((campaign-id campaign-id))
			(
				(description description)
				(link link)
			))
		(ok u1)
		)
	)


(define-public (invest (campaign-id uint) (tier-id uint) (amount uint))
	(let (
		(campaign (unwrap! (map-get? campaigns ((campaign-id campaign-id))) error-campaign-does-not-exist))
		(status (unwrap-panic (map-get? campaign-status ((campaign-id campaign-id)))))
		(total (unwrap-panic (map-get? campaign-totals ((campaign-id campaign-id)))))
		)
		(asserts! (and (< block-height (get target-block-height campaign)) (not (get target-reached status))) error-campaign-inactive)
		(unwrap! (stx-transfer? amount tx-sender (as-contract tx-sender)) error-invest-stx-transfer-failed)
		(let (
			(new-campaign-total (+ (get total-investment total) amount))
			)
			(if (and
					(map-set campaign-totals ((campaign-id campaign-id))
						(
							(total-investment new-campaign-total)
							(total-investors (if (> prior-investment u0) (get total-investors total) (+ (get total-investors total) u1)))
						))
					
				(begin
					(var-set total-investments (+ (var-get total-investments) u1))
					(var-set total-investment-value (+ (var-get total-investment-value) amount))
					(if (>= new-campaign-total (get goal campaign))
						(begin
							(map-set campaign-status ((campaign-id campaign-id))
								(
									(target-reached true)
									(target-reached-height block-height)
									(funded false)
								))
							(var-set total-campaigns-funded (+ (var-get total-campaigns-funded) u1))
							(ok u2) ;; funded and target reached
							)
						(ok u1) ;; else: funded but target not yet reached
						)
					)
				error-general ;; else
				)
			)
		)
	)


(define-public (refund (campaign-id uint) (tier-id uint))
	(let (
		(campaign (unwrap! (map-get? campaigns ((campaign-id campaign-id))) 
        error-campaign-does-not-exist))
		(status (unwrap-panic (map-get? campaign-status ((campaign-id campaign-id)))))
		(total (unwrap-panic (map-get? campaign-totals ((campaign-id campaign-id)))))
		(prior-investment (default-to u0 (get amount (map-get? investments-principal 
        ((campaign-id campaign-id) (investor tx-sender))))))
		(original-tx-sender tx-sender)
		)
		(asserts! (not (get target-reached status)) error-campaign-already-funded)
		(asserts! (> prior-investment u0) error-no-investment)
		(unwrap! (as-contract (stx-transfer? prior-investment tx-sender original-tx-sender)) 
        error-refund-stx-transfer-failed)
		(let (
			(new-campaign-total (- (get total-investment total) prior-investment))
			)
			(if (and
					(map-set campaign-totals ((campaign-id campaign-id))
						(
							(total-investment new-campaign-total)
							(total-investors (- (get total-investors total) u1))
						))
					
					)
				(begin
					(var-set total-investments (- (var-get total-investments) u1))
					(var-set total-investment-value (- (var-get total-investment-value) 
                    prior-investment))
					(ok u1)
					)
				error-general ;; else
				)
			)
		)
	)


(define-public (collect (campaign-id uint))
	(let (
		(campaign (unwrap! (map-get? campaigns ((campaign-id campaign-id))) error-campaign-does-not-exist))
		(status (unwrap-panic (map-get? campaign-status ((campaign-id campaign-id)))))
		(total (unwrap-panic (map-get? campaign-totals ((campaign-id campaign-id)))))
		(original-tx-sender tx-sender)
		)
		(asserts! (is-eq (get fundraiser campaign) tx-sender) error-not-owner)
		(asserts! (not (get funded status)) error-already-funded)
		(asserts! (get target-reached status) error-target-not-reached)
		(unwrap! (as-contract (stx-transfer? (get total-investment total) tx-sender original-tx-sender)) error-funding-stx-transfer-failed)
		(asserts! (map-set campaign-status ((campaign-id campaign-id))
			(
				(target-reached true)
				(target-reached-height (get target-reached-height status))
				(funded true)
			)) error-general)
		(ok u1)
		)
	)