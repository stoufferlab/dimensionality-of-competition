
# extract estimated alpha matrix decomposition for lowest AIC dimensionality fit

# a few utilities we need below
source('./response.effect.from.pars.R')
source('./polar.transform.R')

# read in the absolute best C parameters
load("../../results/Godoy/godoy.C.best.Rdata")
ctraits <- Godoy.best

# read in the absolute best T parameters
load("../../results/Godoy/godoy.T.best.Rdata")
ttraits <- Godoy.best

# # write out response-effect pairs within treatments
# for(d in 1:ncol(ctraits$response)){
# 	write.table(
# 		cbind(ctraits$effect[,d], ctraits$response[,d]),
# 		paste0("../../results/Godoy/godoy.C.traits.response.effect.",d,".csv"),
# 		quote=FALSE,
# 		col.names=FALSE,
# 		sep=" ",
# 		row.names=FALSE
# 	)
# }

# for(d in 1:ncol(ttraits$response)){
# 	write.table(
# 		cbind(ttraits$effect[,d], ttraits$response[,d]),
# 		paste0("../../results/Godoy/godoy.T.traits.response.effect.",d,".csv"),
# 		quote=FALSE,
# 		col.names=FALSE,
# 		sep=" ",
# 		row.names=FALSE
# 	)
# }

# # write out weighted response-effect pairs within treatments
# for(d in 1:ncol(ctraits$response)){
# 	write.table(
# 		cbind(ctraits$effect[,d] * sqrt(ctraits$weights[d]), ctraits$response[,d] * sqrt(ctraits$weights[d])),
# 		paste0("../../results/Godoy/godoy.C.traits.weighted.response.effect.",d,".csv"),
# 		quote=FALSE,
# 		col.names=FALSE,
# 		sep=" ",
# 		row.names=FALSE
# 	)
# }

# for(d in 1:ncol(ttraits$response)){
# 	write.table(
# 		cbind(ttraits$effect[,d] * sqrt(ttraits$weights[d]), ttraits$response[,d] * sqrt(ttraits$weights[d])),
# 		paste0("../../results/Godoy/godoy.T.traits.weighted.response.effect.",d,".csv"),
# 		quote=FALSE,
# 		col.names=FALSE,
# 		sep=" ",
# 		row.names=FALSE
# 	)
# }

# # write out leading response-response and effect-effect pairs across treatments
# write.table(
# 	cbind(ctraits$response[,1], ttraits$response[rownames(ctraits$response),1]),
# 	"../../results/Godoy/godoy.traits.response.response.1.csv",
# 	quote=FALSE,
# 	col.names=FALSE,
# 	sep=" ",
# 	row.names=FALSE
# )

# write.table(
# 	cbind(ctraits$effect[,1], ttraits$effect[rownames(ctraits$effect),1]),
# 	"../../results/Godoy/godoy.traits.effect.effect.1.csv",
# 	quote=FALSE,
# 	col.names=FALSE,
# 	sep=" ",
# 	row.names=FALSE
# )

# # write out fecundities in C and T treatments
# write.table(
# 	cbind(ctraits$lambda, ttraits$lambda),
# 	"../../results/Godoy/godoy.CT.lambdas.csv",
# 	quote=FALSE,
# 	col.names=FALSE,
# 	sep=" ",
# 	row.names=FALSE
# )

# # write out alphas in C and T treatments
# write.table(
# 	cbind(
# 		1/(1+(as.numeric(ctraits$alphas))),
# 		1/(1+(as.numeric(ttraits$alphas)))
# 	),
# 	"../../results/Godoy/godoy.CT.alphas.csv",
# 	quote=FALSE,
# 	col.names=FALSE,
# 	sep=" ",
# 	row.names=FALSE
# )

# write out the separable alpha matrices for both treatments
for(d in 1:ncol(ctraits$response)){
	alphas <- ctraits$response[,d,drop=FALSE] %*% t(ctraits$effect[,d,drop=FALSE]) * ctraits$weights[d]
	write.table(
		alphas,
		paste0("../../results/Godoy/godoy.C.best.alphas.",d,".csv"),
		quote=FALSE,
		col.names=FALSE,
		sep=" ",
		row.names=FALSE
	)
}
write.table(
	ctraits$alphas,
	paste0("../../results/Godoy/godoy.C.best.alphas.csv"),
	quote=FALSE,
	col.names=FALSE,
	sep=" ",
	row.names=FALSE
)

for(d in 1:ncol(ttraits$response)){
	alphas <- ttraits$response[,d,drop=FALSE] %*% t(ttraits$effect[,d,drop=FALSE]) * ttraits$weights[d]
	write.table(
		alphas,
		paste0("../../results/Godoy/godoy.T.best.alphas.",d,".csv"),
		quote=FALSE,
		col.names=FALSE,
		sep=" ",
		row.names=FALSE
	)
}
write.table(
	ttraits$alphas,
	paste0("../../results/Godoy/godoy.T.best.alphas.csv"),
	quote=FALSE,
	col.names=FALSE,
	sep=" ",
	row.names=FALSE
)
