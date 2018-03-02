naiveFormula = formula(loss~.-id)
naiveOLS = lm(naiveFormula, data = pml.train)
