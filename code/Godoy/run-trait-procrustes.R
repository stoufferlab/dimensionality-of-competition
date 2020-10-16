
# conduct procrustes tests between species-species trait matrices

# empty containers for the traits
control.traits <- list(
	response = NULL,
	effect = NULL
)
treatment.traits <- list(
	response = NULL,
	effect = NULL
)

# read in the control response traits
control.traits$response <- data.frame(T1=read.table("../../results/Godoy/godoy.C.response.1.csv")[,1])
control.traits$response$T2 <- read.table("../../results/Godoy/godoy.C.response.2.csv")[,1]
control.traits$response$T3 <- read.table("../../results/Godoy/godoy.C.response.3.csv")[,1]

# read in the control effect traits
control.traits$effect <- data.frame(T1=read.table("../../results/Godoy/godoy.C.effect.1.csv")[,1])
control.traits$effect$T2 <- read.table("../../results/Godoy/godoy.C.effect.2.csv")[,1]
control.traits$effect$T3 <- read.table("../../results/Godoy/godoy.C.effect.3.csv")[,1]

# read in the treatment response traits
treatment.traits$response <- data.frame(T1=read.table("../../results/Godoy/godoy.T.response.1.csv")[,1])
treatment.traits$response$T2 <- read.table("../../results/Godoy/godoy.T.response.2.csv")[,1]
treatment.traits$response$T3 <- read.table("../../results/Godoy/godoy.T.response.3.csv")[,1]

# read in the treatment effect traits
treatment.traits$effect <- data.frame(T1=read.table("../../results/Godoy/godoy.T.effect.1.csv")[,1])
treatment.traits$effect$T2 <- read.table("../../results/Godoy/godoy.T.effect.2.csv")[,1]
treatment.traits$effect$T3 <- read.table("../../results/Godoy/godoy.T.effect.3.csv")[,1]

## all traits
proc.test <- vegan::protest(
	dist(cbind(control.traits$response, control.traits$effect)),
	dist(cbind(treatment.traits$response, treatment.traits$response))
)
print(proc.test)

## response traits
proc.test.response <- vegan::protest(
	dist(control.traits$response),
	dist(treatment.traits$response)
)
print(proc.test.response)

## all traits
proc.test.effect <- vegan::protest(
	dist(control.traits$effect),
	dist(treatment.traits$effect)
)
print(proc.test.effect)