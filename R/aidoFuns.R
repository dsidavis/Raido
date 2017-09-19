#
# Todo
#  When we get the outbreaks as a data frame, then go and fill the location information on the unique location ids.
#
#  convert the id column of a data frame to a factor/string, not an integer. Same with location
#
#  Collapse the result of the admin_level (getLocationAdmin) into a data frame.
#  ?? Let location in getOutbreaks() be a human-readable description and then map this a location id.
#    possibly mutiple matches. Use the first one and HOPE!
#
#  vectorize getDisease() and getLocation()
#
#  [done] allow the caller of getOutbreaks to specify the disease by name and we map it to an id.
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
function(disease, location = character(), max = Inf,
         url = "http://aido.bsvgateway.org/api/outbreaks",
         convertFun = outbreaks2DataFrame,             
         curl = getCurlHandle(..., followlocation = TRUE), ...)
{
    if(!grepl("^[0-9]+$", disease))
       disease = getDiseaseID(disease)
    
    args = list(disease = disease)
    if(length(location)) {
        if(!grepl("^[0-9]+$", location))
            location = getLocation(location, curl = curl)[[1]]$id
        args$location = location
    }
    
    txt = getForm(url, .params = args, curl = curl)
    processPages(txt, curl, max = max, convertFun = convertFun)
}



processPages =
    #
    # go through all the "next" pages after the initial result and cumulate the results.
    # max allows the caller to limit this to a total number of individual results.
    #
    # There are (currently) 10 results per page.
    #
function(tmp, curl, max = Inf, convertFun = NULL, ...)
{
    if(is.character(tmp))
       tmp = fromJSON(tmp)

    if(!("results" %in% names(tmp)))
       return(tmp)
       
    ans = tmp$results
    while(length(ans) < max && length(tmp$"next")) {
        tmp = fromJSON(getURLContent(tmp$"next", curl = curl))
        ans = c(ans, tmp$results)
    }

    if(is.null(convertFun))
       ans
    else
       convertFun(ans, ...)
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

    ans = processPages(txt, curl, max = max)
#    convert2DataFrame(ans, names(ans[[1]]))
}


getLocationAdmin =
function(level, max = Inf, url = "http://aido.bsvgateway.org/api/locations/",
         curl = getCurlHandle(..., followlocation = TRUE), ...)
{
    processPages(getForm(url, admin_level = level, curl = curl), curl, max = max)
}

getDiseases =
    #
    #'@title get all the disease name and id pairs in the database
    #
function(max = Inf, url = "http://aido.bsvgateway.org/api/diseases",
         curl = getCurlHandle(..., followlocation = TRUE), ...)
{
    ans = processPages(getURLContent(url, curl = curl), curl, max = max)
    convertDisease(ans, dataFrame = FALSE)
}

convertDisease =  convert2DataFrame =
function(d, vars = c("id", "name"), dataFrame = TRUE)
{
   tmp = lapply(vars, function(x) sapply(d, `[[`, x))    
   if(dataFrame) {
        ans = as.data.frame(tmp, stringsAsFactors = FALSE)
        names(ans) = vars
    } else 
        ans = structure(tmp[[1]], names = tmp[[2]])

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
