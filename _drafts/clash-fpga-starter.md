---
layout: post
title:  "CλaSH FPGA Starter"
date:   2014-08-20 09:10:00
categories: draft
tags: blogging draft
comments: false
analytics: false
---

`Blinker.hs`:
{% highlight haskell %}
module Blinker where

import CLaSH.Prelude

topEntity :: Signal Bit -> Signal (BitVector 8)
topEntity mode = leds
  where
    modeR = isRising 1 mode
    leds  = mealy blinkerT (1,False,0) modeR

blinkerT (leds,mode,cntr) modeR = ((leds',mode',cntr'),leds)
  where
    -- clock frequency = 50e6  (50 MHz)
    -- led rate        = 333e6 (change rate is 333ms)
    cnt_max = 16650000 -- 50e6 * 333e6

    cntr' | cntr == cnt_max = 0
          | otherwise       = cntr + 1

    mode' | modeR     = not mode
          | otherwise = mode

    leds' | cntr == 0 = if mode then complement leds
                                else rotateL leds 1
          | otherwise = leds
{% endhighlight %}

`blinker.topentity`:
{% highlight json %}
{ "TopEntity" :
  { "name"     : "blinker"
  , "inputs"   : [ "MODE" ]
  , "outputs"  : [ "LED" ]
  , "extra_in" : [ ["CLOCK_50", 1 ]
                 , ["KEY", 1 ]
                 ]
  , "clocks"   :
    [ { "Source" :
        { "name"  : "altpll50"
        , "paths" : [ { "Path" :
                        { "inp"  : ["inclk0", "CLOCK_50(0)"]
                        , "outp" : [ ["c0","System"] ]
                        }
                      }
                    ]
        , "clear" : ["areset","not KEY(0)"]
        , "lock"  : "locked"
        }
      }
    ]
  }
}
{% endhighlight %}

`blinker.qsf`:

```
#============================================================
# FPGA
#============================================================
set_global_assignment -name FAMILY "Cyclone IV E"
set_global_assignment -name DEVICE EP4CE22F17C6
set_global_assignment -name TOP_LEVEL_ENTITY "blinker"
set_global_assignment -name DEVICE_FILTER_PACKAGE FBGA
set_global_assignment -name DEVICE_FILTER_PIN_COUNT 256
set_global_assignment -name DEVICE_FILTER_SPEED_GRADE 6
set_global_assignment -name SDC_FILE blinker.SDC

#============================================================
# CLOCK
#============================================================
set_location_assignment PIN_R8 -to CLOCK_50[0]
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to CLOCK_50[0]

#============================================================
# LED
#============================================================
set_location_assignment PIN_A15 -to LED[0]
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to LED[0]
set_location_assignment PIN_A13 -to LED[1]
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to LED[1]
set_location_assignment PIN_B13 -to LED[2]
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to LED[2]
set_location_assignment PIN_A11 -to LED[3]
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to LED[3]
set_location_assignment PIN_D1 -to LED[4]
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to LED[4]
set_location_assignment PIN_F3 -to LED[5]
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to LED[5]
set_location_assignment PIN_B1 -to LED[6]
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to LED[6]
set_location_assignment PIN_L3 -to LED[7]
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to LED[7]

#============================================================
# KEY
#============================================================
set_location_assignment PIN_J15 -to KEY[0]
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to KEY[0]
set_location_assignment PIN_E1 -to MODE[0]
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to MODE[0]
```

`blinker.sdc`:

```
#**************************************************************
# Create Clock
#**************************************************************
create_clock -period 20 [get_ports CLOCK_50]

#**************************************************************
# Create Generated Clock
#**************************************************************
derive_pll_clocks
```

