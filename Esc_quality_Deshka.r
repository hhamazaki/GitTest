################################################################################
# ASL data Analyses R-code
################################################################################
################################################################################
#  0.0  Initialize working Environment                                          
################################################################################
rm(list=ls(all=TRUE))
# Include mgcv library
library(reshape2)
# Specify working directory: Where your file is located 
working_dir <- 'c:/Projects/Escapement_quality/Deshka/'
# Enter the name of data file  
data_file1 <- 'Deshka_ASL.csv'
data_file2 <- 'Deshka_BRD.csv'
data_file3 <- 'Deshka_BRD.csv'
data_file4 <- 'Deshka_BRD.csv'

#'------------------------------------------------------------------------------
#   1.0 Read source file 
#'------------------------------------------------------------------------------
source('C:/Projects/Escapement_Quality/R_functions/Quality_SRA_functions.R')
source('C:/Projects/Escapement_Quality/R_functions/JAGS_SR.R')
#'------------------------------------------------------------------------------
# 1.1 read ASL table and clean up for analyses. 
#'------------------------------------------------------------------------------
# Clean data and combine test and comfish data
o2.size <- read.csv(file.path(working_dir,data_file1), na.strings ='', stringsAsFactors =FALSE,header = TRUE )

# Define size
o.size$large <- ifelse(o.size$Length.mean >= 750,1,0) 
ml <- aggregate(age.p~Year, FUN=sum, data=o.size)
ml <- merge(o.size,ml,by=c('Year'))
ml$p.age <- with(ml,age.p.x/age.p.y)
Esc.p <- ml
# Combine        
Esc.p$pfm <- Esc.p$p.age
Esc.p$pf <- with(Esc.p,ifelse(Sex=='F',pfm,0))
Esc.p$p.l <- with(Esc.p,large*pfm)
# Fecundity Fleishman and Reimer (2017) Kenai
Esc.p$fec <- with(Esc.p,ifelse(Sex=='F',0.01111*Length.mean^1.981,0))
Esc.p$p.fec <- with(Esc.p,fec*pfm)
Esc.p$flen <- with(Esc.p,ifelse(Sex=='F',Length.mean*pfm,0))
Esc.quality<- aggregate(cbind(flen,pf,p.l,p.fec)~Year, sum, data=Esc.p)
Esc.quality$flen <- with(Esc.quality,flen/pf)
 names(Esc.quality) <- c('Year','flen','pfem','pl','fec')
Esc.quality	 
#write.csv(Esc.quality,file.path(working_dir,'Deshka_Esc_quality.csv',sep=''),na='',row.names=FALSE) 

#'-----------------------------------------------------------------------------
#' Read Brood data 
#'------------------------------------------------------------------------------
# Set directory where data file is located
brood <- read.csv(file.path(working_dir,data_file2), na.strings ='', stringsAsFactors =FALSE,header = TRUE)
brood <- merge(brood[,c('Year','S','R')], Esc.quality, by = c('Year'))
brood <- brood[complete.cases(brood),]
names(brood) <- c('Year','S','R','fl','pfem','pl','fec')

#write.csv(brood,file.path(working_dir,'Deshka_brood.csv',sep=''),na='',row.names=FALSE) 


brood$Sf <- with(brood, S*pfem)
brood$Sl<- with(brood, S*pl)
brood$Se <- with(brood, S*fec)
brood$rfem <- with(brood,pfem/mean(pfem))
brood$rlg <- with(brood,pl/mean(pl))
brood$rfec <- with(brood,fec/mean(fec))

datnew<-list()
datnew$nyrs=length(brood$Year)
datnew$S=brood$S
datnew$R=brood$R
datnew$f=brood$rfec
datnew$SE=brood$Se
datnew$d = floor(log10(mean(brood$S)))
datnew$D = floor(log10(mean(brood$Se)))
mod <- data.frame(m1 = c(1,0,1,0,0),m2=c(1,0,1,0,1),m3=c(0,1,1,0,0),m4=c(0,1,0,1,0),m5=c(1,0,0,1,0))
ar1 <- 0

jag.out <-JAGS_SR (datanew, ar1,mod, working_dir)
saveRDS(jag.out,file=file.path(working_dir,'Deshka_jag.RDS'))
readRDS(file=file.path(working_dir,'Deshka_jag.RDS'))


















model_out <- model_run(brood,FALSE)
fit_out(model_out,FALSE)

model.pars <- rout(brood,Esc.quality,1)
br <- BR_out(model_out,model.pars)


S <- seq(0,1.5*max(brood$S),length.out=201)		
RS <- with(br,exp(ln.alpha[1])*S*exp(-beta[1]*S))
Smsy.RS <- br$Smsy[1]
RS.1 <- with(br,exp(ln.alpha[4])*S*exp(-beta[4]*S))
Smsy.RS.1 <-br$Smsy[4] 
RS.2 <- with(br,exp(ln.alpha[3])*S*exp(-beta[3]*S))
Smsy.RS.2 <-br$Smsy[3] 
RS.3 <- with(br,exp(ln.alpha[2])*S*exp(-beta[2]*S))
Smsy.RS.3 <-br$Smsy[2] 
plot(R~S,xlim=c(0,max(S)), ylim=c(0,max(R)),pch=19, data=brood)
lines(S,RS)
lines(S,RS.1, col=2)
lines(S,RS.2, col=3)
lines(S,RS.3, col=4)
abline(0,1)
legend('topright',c('R=a*S*exp(-bS)','R=a*E*exp(-bE)','R=a*S*exp(-bS)*exp(g)*r','R=a*Eexp(-bS)'),lwd=1,col=c(1,2,3,4),bty='n')
abline(v=c(Smsy.RS,Smsy.RS.1,Smsy.RS.2,Smsy.RS.3),col=c(1,2,3,4),lty=2)















