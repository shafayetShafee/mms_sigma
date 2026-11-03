#!/bin/bash

cat << 'EOF' > /home/rstudio/.Rprofile
# Source - https://stackoverflow.com/a/78001912
# Posted by gladys_c_hugh
# Retrieved 2025-11-11, License - CC BY-SA 4.0
# Then modified accordingly

setHook(
  hookName = "rstudio.sessionInit",
  value = function(newSession) {
    if (newSession && is.null(rstudioapi::getActiveProject()))
      rstudioapi::openProject("/home/rstudio/mms_sigma/mms_sigma.Rproj")
    },
  action = "append"
)

EOF
