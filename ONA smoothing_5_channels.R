#ONA smoothing 5 channels: returns ONA corrected BC readings for 5 channels: UVONA, BlueONA, GreenONA, RedONA, IRONA
#Version 2
#February 2019
#Original file from Elena Austin

library(pacman)
p_load("data.table", "zoo", "plyr")

makenum = function(x) {as.numeric(as.character(x))}

ONAsmoothing<-function(MAdata, 
                       deltaATN=data.table(UV=0.05, 
                                           Blue = 0.05, 
                                           Green = 0.05, 
                                           Red = 0.05, 
                                           IR = 0.05)) {
  
#Elena: The tape position is to make sure measured values within the same attenuation threshold but different tape positions are not averaged. If you have measurements at the same position over multiple days, then I would make sure to add a term in the 'by' specification. Something like: by = c("Tape position", "day")

MAdata[ , UVthresh := round_any((`UV ATN1`)-max(`UV ATN1`)-deltaATN$UV, (deltaATN$UV+0.01), ceiling) + max(`UV ATN1`), by = "Tape position"]   
MAdata[ , UVONA := mean(makenum(`UV BC1`)), by = c("Tape position", "UVthresh")] 

MAdata[ , Bluethresh := round_any((`Blue ATN1`)-max(`Blue ATN1`)-deltaATN$Blue, (deltaATN$Blue+0.01), ceiling) + max(`Blue ATN1`), by = "Tape position"]    
MAdata[ , BlueONA := mean(makenum(`Blue BC1`)), by = c("Tape position", "Bluethresh")] 

MAdata[ , Greenthresh := round_any((`Green ATN1`)-max(`Green ATN1`)-deltaATN$Green, (deltaATN$Green+0.01), ceiling) + max(`Green ATN1`), by = "Tape position"]    
MAdata[ , GreenONA := mean(makenum(`Green BC1`)), by = c("Tape position", "Greenthresh")] 

MAdata[ , Redthresh := round_any((`Red ATN1`)-max(`Red ATN1`)-deltaATN$Red, (deltaATN$Red+0.01), ceiling) + max(`Red ATN1`), by = "Tape position"]    
MAdata[ , RedONA := mean(makenum(`Red BC1`)), by = c("Tape position", "Redthresh")] 

MAdata[ , IRthresh := round_any((`IR ATN1`)-max(`IR ATN1`)-deltaATN$IR, (deltaATN$IR+0.01), ceiling) + max(`IR ATN1`), by = "Tape position"]    
MAdata[ , IRONA := mean(makenum(`IR BC1`)), by = c("Tape position", "IRthresh")] 

#summary(MAdata[, length(`UV ATN1`), by= c("Tape position", "UVthresh")]$V1)

# MAdata[, UVthresh := NULL]
MAdata

}