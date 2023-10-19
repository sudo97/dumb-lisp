(do
    (define fac (fun (n)
        (let fac_tailcall (fun (x acc)
            (? (= x 1)
                acc
                (fac_tailcall (- x 1) (* x acc)
                )
            )
        )
        (fac_tailcall n 1))
    ))
)