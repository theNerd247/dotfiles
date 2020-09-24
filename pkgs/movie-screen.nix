self: super:

{ movie-screen = super.writeScriptBin "movie-screen"
    ''
    #!${self.bash}/bin/bash

    # turn off Display Power Management Service (DPMS)
    ${self.xorg.xset}/bin/xset -dpms
    setterm -blank 0 -powerdown 0

    # turn off black Screensaver
    ${self.xorg.xset}/bin/xset s off
    '';
}
