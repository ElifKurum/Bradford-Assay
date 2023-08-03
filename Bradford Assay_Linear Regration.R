#....Micro Bradford Assay için R Studio’da Doğrusal Regresyon Grafiği Oluşturma (Tablo 1)
#
#....Kaynak - https://rforbiochemists.blogspot.com/2015/04/lets-draw-graph-of-protein-assay.html
#
#....Elimizde 1, 5, 10, 15, 20 ve 25 Konsantrasyonlarında ölçülmüş değerler var 
#....3 tekrar yaptığımız için her bir konsantrasyonda elde ettiğimiz 3 farklı A595nm değerinin 
#.......ortalamasını alacağız.
#....Bunun için önce vektör oluşturup sonra mean fonksiyonundan yararlanacağız.
#....Kendi bulduğunuz değerleri aşağıdaki c parantezi içine yazmalısınız.
#....Protokolde yer alan değerleri girdim deneme amaçlı ancak ordaki grafikte 8 farklı değer
#.......bulunduğu için R square değeri farklılık gösterdi.
#
#
#....Vektör Oluşturma:
A1 <- c(0.066,0.068,0.067)
A5 <- c(0.224,0.225,0.222)
A10 <- c(0.402,0.410,0.406)
A15 <- c(0.573,0.569,0.570)
A20 <- c(0.720,0.705,0.710)
A25 <- c(0.851,0.892,0.860)
#
#....Ortalama Bulma:
MA1 <- mean(A1)
MA5 <- mean(A5)
MA10 <- mean(A10)
MA15 <- mean(A15)
MA20 <- mean(A20)
MA25 <- mean(A25)
#
#....Ortalama Değerlerimizi Bulduk
#....Şimdi yukarıdaki kaynaktan faydalanacağız
#....y eksenimiz absorbsiyon, x eksenimiz konsantrasyon
#....Protein Konsantrasyonları için tekrar vektör oluşturuyoruz
#
#....Protein Concentrations
PC <- c(1,5,10,15,20,25) 

#....Protein Absorbtion
PA <- c(MA1,MA5,MA10,MA15,MA20,MA25)

#....Basit bir grafik çizelim (abs-PC, prot-PA)
plot(PA~PC)

#....Plotumuz sağ alt frame de görülüyor ancak bazı eklemeler yapmamız lazım
#....Bu yüzden kaynakta yer alan verileri kendi istediğimiz verilere göre düzenliyoruz

#Calculate the line using the linear model function
line <- lm(PA~PC)

#Draw the line
abline(line)

#Improve the graph:
plot(PA~PC, 
     xlab = "c(BSA) [microg/ml]",
     ylab = "Absorbance (595nm)",
     main = "Calibration Curve Bradford Micro Assay 3rd August 2023")
abline(line)

#....Bundan sonrasını bilgi birikimimi aşıyor
r2 <- round(summary(line)$r.squared, 3)
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 20, y = 0.3, labels = mylabel)

#Equation of a line y = mx + c
#In our case abs = slope * prot + intercept
# ukn.prot = (abs - intercept)/slope
int <- summary(line)$coefficients[1]
slope <- summary(line)$coefficients[2]
mylabel = bquote(y == .(format(slope, digits = 3))*x + .(format(int, digits = 3)))
text(x = 20, y = 0.4, labels = mylabel)

#....BİLİNMEYEN PROTEİN ABSORBSİYONLARI İÇİN KODLAR
#....AŞAĞIDAKİ abs.unknown kısmına kendi proteinlerimizin değerlerini yazacağız
#....burada 0.554,0.568 ve 0.705 örnek bilinmeyen protein değerleri
#now calculate some unknown protein concs from absorbances
#put the unknowns into a vector
abs.unknowns <- c(0.554, 0.568, 0.705)
#rearrange the equation of the line to ukn.prot = (abs - intercept)/slope
prot.unknowns <- (abs.unknowns - int)/slope
#put the answers on the graph
text(x = 5, y = (0.7), "Abs")
text(x = 10, y = (0.7), "Prot")
for (i in 1:length(abs.unknowns)){
  text(x = 5, y = (0.7 - i/20), abs.unknowns[i])
  text(x = 10, y = (0.7 - i/20), round(prot.unknowns[i], 3))
}

#....İstenildiği taktirde grafikte estetik değişiklikler yapılabilir.
#....END OF SCRIPT



