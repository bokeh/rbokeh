
s1 <- jsonlite::fromJSON("code-gen/spec_0.12.6.json", simplifyVector = FALSE)
s2 <- jsonlite::fromJSON("code-gen/spec_0.12.9.json", simplifyVector = FALSE)


sort(sapply(s1$HoverTool$props, "[[", "name"))
sort(sapply(s2$HoverTool$props, "[[", "name"))

digest::digest(sort(sapply(s1$HoverTool$props, "[[", "name")))
digest::digest(sort(sapply(s2$HoverTool$props, "[[", "name")))
