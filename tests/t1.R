library(Raido)

if(url.exists("http://aido.bsvgateway.org/api/"))
{

    id = getDiseaseID(c("yellow", "zika")))
    stopifnot(all(id == c(61, 68))
          
    getDiseases()

    o = getOutbreaks(28)

    eb1 = getOutbreaks("Ebola", "gabon")
    stopifnot(is.data.frame(eb1))

    a = getLocation(admin = 2, max = 20)
    stopifnot(all(dim(a) == c(20, 9)))

    l1 = getLocation("26102600")   # 96271664
    l2 = getLocation("https://aido.bsvgateway.org/api/locations/26102600/")
    stopifnot( identical(l1, l2) )
}
