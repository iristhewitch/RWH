-- file: ch03/guard.hs

fromMaybe defval wrapped = 
    case wrapped of
        Nothing -> defval
        Just value -> value