#
# Todo
#  allow the caller of getOutbreaks to specify the disease by name and we map it to an id.
#  When we get the outbreaks as a data frame, then go and fill the location information on the unique location ids.
#
#  Collapse the result of the admin_level (getLocationAdmin) into a data frame.
#  ?? Let location in getOutbreaks() be a human-readable description and then map this a location id.
#
#  vectorize getDisease() and getLocation()
#
#  [done]check the admin_level - add process $"next" for pages
#  [done] in getLocation() allow the caller to give us the URL from the result rather than location id.
#  [done] convert the times to Date (or POSIXct)
#  [done] get the diseases locally so we don't have to perform an online request.
#
#
getOutbreaks =
    #
    # For a given disease, specified either by its identifier or (initial part) of its English name
    # get all of the corresponding outbreaks.  This returns the
    # One can also specify a location, currently only by its identifier.
    # 
function(disease, location = character(), max = Inf, url = "http://aido.bsvgateway.org/api/outbreaks",
         convertFun = outbreaks2DataFrame,             
          curl = getCurlHandle(..., followlocation = TRUE), ...)
{
    if(!grepl("^[0-9]+$", disease))
       disease = getDiseaseID(disease)
    
    args = list(disease = disease)
    if(length(location)) {
        if(!grepl("^[0-9]+$", location))
            location = getLocation(location, curl = curl)$id
        args$location = location
    }
    txt = getForm(url, .params = args, disease = disease, curl = curl)
    tmp = fromJSON(txt)

    ans = tmp$results
    while(length(ans) < max && length(tmp$"next")) {
        tmp = fromJSON(getURLContent(tmp$"next", curl = curl))
        ans = c(ans, tmp$results)
    }

    if(is.null(convertFun))
       ans
    else
       convertFun(ans)
}



processPages =
    #
    # get the next pages after the initial reslt.
    #
function(tmp, curl, max = Inf)
{
    if(is.character(tmp))
       tmp = fromJSON(tmp)
    
    ans = tmp$results
    while(length(ans) < max && length(tmp$"next")) {
        tmp = fromJSON(getURLContent(tmp$"next", curl = curl))
        ans = c(ans, tmp$results)
    }
    ans
}

if(FALSE) {
    o = getOutbreaks(28)
    a = lapply(o, function(v) convert2DataFrame(v$time_series, c("end", "start", "value")))
    dd = convert2DataFrame(o, names(o[[1]])[1:5])
}

outbreaks2DataFrame =
function(o)
{
    d2 = lapply(o, function(v) {
           df = convert2DataFrame(v$time_series, c("end", "start", "value"))
           df$id = v$id
           df$disease = v$disease
           df$location = v$location
           df$duration = v$duration
           df
       })
    ans = do.call(rbind, d2)

    v = c("start", "end")
    ans[v] = lapply(v, function(x) as.Date(strptime(ans[[x]], "%Y-%m-%dT%H:%M:%S")))

    v = c("disease", "location")
    ans[v] = lapply(v, function(x) gsub(".*/", "", gsub("/$", "", ans[[x]])))
    
    ans
}


if(FALSE)
getLocationID =
function(search, url = "http://aido.bsvgateway.org/api/locations/",
         curl = getCurlHandle(..., followlocation = TRUE), ...)
{
    fromJSON(getForm(url, search = search, curl = curl))
}

getLocation =
    #
    # getLocation(admin = 1)
    #
    # getLocation("26102600")
    # getLocation("https://aido.bsvgateway.org/api/locations/26102600/")
    #
function(location = character(), admin = integer(), max = Inf,
         url = "http://aido.bsvgateway.org/api/locations/",
         curl = getCurlHandle(..., followlocation = TRUE), ...)
{
    if(!missing(admin))
        return(getLocationAdmin(admin, max = max, url = url, curl = curl))
    
     txt = if(grepl("^http.*/[0-9]+/?$", location))
            # if the location contains the location identifier, then this is the API URL to query directly.        
              getURLContent(location, curl = curl)
           else if(!grepl("^[0-9]+$", location))
              getForm(url, search = location, curl = curl)
           else 
              getURLContent(paste0(url, location), curl = curl)

    ans = processPages(txt, curl, max)
    convert2DataFrame(ans, names(ans[[1]]))
}


getLocationAdmin =
function(level, max = Inf, url = "http://aido.bsvgateway.org/api/locations/",
         curl = getCurlHandle(..., followlocation = TRUE), ...)
{
    tmp = fromJSON(getForm(url, admin_level = level, curl = curl))
    ans = tmp$results
    while(length(ans) < max && length(tmp$"next")) {
        tmp = fromJSON(getURLContent(tmp$"next", curl = curl))
        ans = c(ans, tmp$results)
    }
#    do.call(rbind, lapply(ans, as.data.frame, stringsAsFactors = FALSE))
    ans
}

getDiseases =
    #
    # get all the disease name and id pairs in the database
    #
function(max = Inf, url = "http://aido.bsvgateway.org/api/diseases",
         curl = getCurlHandle(..., followlocation = TRUE), ...)
{
    tmp = fromJSON(getURLContent(url, curl = curl))
    ans = tmp$results
    while(length(ans) < max && length(tmp$"next")) {
        tmp = fromJSON(getURLContent(tmp$"next", curl = curl))
        ans = c(ans, tmp$results)
    }
    convertDisease(ans, dataFrame = FALSE)
}

convertDisease =  convert2DataFrame =
function(d, vars = c("id", "name"), dataFrame = TRUE)
{
   tmp = lapply(vars, function(x) sapply(d, `[[`, x))    
   if(dataFrame) {
        ans = as.data.frame(tmp, stringsAsFactors = FALSE)
        names(ans) = vars
    } else {
        ans = structure(tmp[[1]], names = tmp[[2]])
    }
    ans
}


getDiseaseID =
    #
    #'@title map a human-readable disease name to the corresponding id in the database.
    #'@local: a logical value determining whether we use the previously downloaded mapping of names to ids, or if we 
    #   make the request now. The former is faster; the latter is more up-to-date.
    # @diseases - the named vector of disease identifiers with the names being the human-readable form.
    #
function(name, local = TRUE,
         diseases = if(local) diseaseMap else getDiseases(curl = curl),
         curl = getCurlHandle(..., followlocation = TRUE), ...)
{
    i = pmatch(tolower(name), tolower(names(diseases)))
    if(all(is.na(i)))
        stop("No matching disease(s): ", paste(name, collapse = ", "))
    else if(any(is.na(i)))
        warning("No matching disease: ", name[is.na(i)])

    diseases[i[!is.na(i)]]
}
