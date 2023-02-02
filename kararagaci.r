#veri setimizi çağıralım.
diyabet <- read.table(file.choose(), header = T, sep = ";")
head(diyabet)
str(diyabet)

#hedefnitelik faktore ceviriyoruz
levels(diyabet$Diabetes) <- c("No","Yes")
diyabet$Diabetes <- as.factor(diyabet$Diabetes)

#özetine bakalım.
str(diyabet)

#karar ağacı
head(diyabet)
Agac <- J48(diyabet$Diabetes ~.,data = diyabet)
Agac
summary(Agac)
plot(Agac)

#ongoru
ongoru <- predict(Agac)
ongoru [1:100]

data.frame(data=diyabet$Diabetes, ongoru=ongoru)[1:404,]

#modelin karışıklık matrisi 
table(diyabet$Diabetes, ongoru)
#isim koyalım
karisiklikmatrisi <- table(diyabet[,11],ongoru)
karisiklikmatrisi

#modelin performans değerlendirme ölçütleri
#doğru pozitif
(TP <- karisiklikmatrisi [1])
#yanlış pozitif
(FP <- karisiklikmatrisi [3])
#yanlış negatif
(FN <- karisiklikmatrisi [2])
#doğru negatif
(TN <- karisiklikmatrisi [4])

#performans degerlendirme olcutleri
paste0("Dogruluk = ",(Dogruluk <- (TP+TN)/sum(karisiklikmatrisi)))
paste0("Hata = ",(Hata <- 1-Dogruluk))
#TPR=Duyarlilik orani
paste0("TPR= ", (TPR <- TP/(TP+FN)))
#SPC=Belirleyicilik orani
paste0("SPC= ", (SPC <- TN/(FP+TN)))
#PPV=kesinlik ya da pozitif ongoru degeri
paste0("PPV= ", (PPV <- TP/(TP+FP)))
#NPV=negatif ongoru degeri
paste0("NPV= ", (NPV <- TN/(TN+FN)))
#FPR=Yanlis pozitif orani
paste0("FPR = ", (FPR <- FP/sum(karisiklikmatrisi)))
#FNR=Yanlis negatif orani
paste0("FNR=",(FNR <- FN/(FN+TP)))
#F olcutu kesinlik ve duyarlilik olcutlerinin harmonik ortalamasi
paste0("F_measure = ", (F_measure <- (2*PPV*TPR)/(PPV+TPR)))