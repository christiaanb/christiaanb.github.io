---
layout: post
title:  "CλaSH FPGA Starter"
date:   2015-05-01 16:15:00
categories: draft
tags: cλash haskell fpga
comments: true
analytics: true
---
In this tutorial we'll be programming the Terasic [DE0-Nano](http://www.terasic.com.tw/cgi-bin/page/archive.pl?Language=English&CategoryNo=165&No=593&PartNo=1) FPGA development board using the functional hardware description language [CλaSH](http://www.clash-lang.org).
The end result of this tutorial is demonstrated in the video below:

<div class="youtube-video">
  <iframe width="320" height="180" src="https://www.youtube.com/embed/XUj9cewMsww" frameborder="0" allowfullscreen></iframe>
</div>

This tutorial is not a general introduction to CλaSH, nor to programming FPGAs.
It is meant to demonstrate how to use of `.topentity` files (added in version `0.5.4` of CλaSH) to configure your CλaSH designs for an FPGA, without writing a single line of VHDL or (System)Verilog.
Even then, this tutorial is already almost too long for single blog post, but here goes:

<br>

## Blinker circuit

We start with some general information about the [DE0-Nano](http://www.terasic.com.tw/cgi-bin/page/archive.pl?Language=English&CategoryNo=165&No=593&PartNo=1) board:

* It has a 50 MHz crystal connected to a clock pin of FPGA, which we will connect to one of the [PLL](https://www.altera.com/literature/ug/ug_altpll.pdf)s that is on the FPGA to create a _stable_ 50 MHz clock signal.
* It has 8 green LEDs that turn on when we put a `1` on the FPGA pin connected to the LED.
* It has two buttons which are already properly [debounced](http://en.wikipedia.org/wiki/Switch#Contact_bounce) by a Schmitt trigger. The buttons are `0` when they are pressed, and `1` when they are not.

The circuit that we are making will repeatedly do one of two things:

* Invert the state of the LEDs
* Rotate the state of the LEDs

We switch between these two modes when the `KEY1` button on the board is pressed _and subsequently released_.
We reset the circuit by pressing the `KEY0` button.

## CλaSH circuit specification

The `Blinker.hs` file given below implements the behaviour of circuit (_sans_ reset behaviour) we just described.
As the clock is running at `50 MHz`, we shouldn't update the state of the LEDs every clock cycle, as they will just seem to to all be turned on all the time to the human eye.
We hence use a counter that counts up to a certain amount of clock cycles, and only update the state of the LEDs when the desired number of cycles is reached and the count is reset.

`Blinker.hs`:
{% highlight haskell %}
module Blinker where

import CLaSH.Prelude

topEntity :: Signal Bit -> Signal (BitVector 8)
topEntity key1 = leds
  where
    key1R = isRising 1 key1
    leds  = mealy blinkerT (1,False,0) key1R

blinkerT (leds,mode,cntr) key1R = ((leds',mode',cntr'),leds)
  where
    -- clock frequency = 50e6   (50 MHz)
    -- led update rate = 333e-3 (every 333ms)
    cnt_max = 16650000 -- 50e6 * 333e-3

    cntr' | cntr == cnt_max = 0
          | otherwise       = cntr + 1

    mode' | key1R     = not mode
          | otherwise = mode

    leds' | cntr == 0 = if mode then complement leds
                                else rotateL leds 1
          | otherwise = leds
{% endhighlight %}

## `topentity` configuration

Now that we've created our CλaSH design, it's time to move on to the important part of this tutorial, the specification of the `.topentity` file.
The `.topentity` file should have the same name, as the `module` name in which our `topEntity` function is located; so in this case:

`blinker.topentity`:
{% highlight json %}
{ "TopEntity" :
  { "name"     : "blinker"
  , "inputs"   : [ "KEY1" ]
  , "outputs"  : [ "LED" ]
  , "extra_in" : [ ["CLOCK_50", 1 ]
                 , ["KEY0", 1 ]
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
        , "reset" : ["areset","not KEY0(0)"]
        , "lock"  : "locked"
        }
      }
    ]
  }
}
{% endhighlight %}

The `.topentity` file is a [JSON](http://www.json.org/) encoded specification of our top-level component in the hierarchy.
We'll go through the _keys_ step-by-step:

* `name`: the name our component should have. In our case: `blinker`
* `inputs`: a list of names our inputs should have. In our case: `KEY1`
* `outputs`: a list of names our outputs should have. In our case: `LED`
* `extra_in`: a list of extra inputs that do not correspond to the arguments of our `topEntity` function.
  These extra inputs are always bit vectors.
  Every item in the `extra_in` list is a tuple (encoded as a 2-element list) of a name, and the number of bits for that input.
  In our case we add an extra 1-bit input `CLOCK_50` which will correspond to the pin to which the 50MHz crystal is attached, and a 1-bit input `KEY0` which is the button we will use as a reset.
* `clocks`: a list of clock sources.

Every clock `Source` consists of the following:

* `name`: the name of the component generating our clock. In our case `altpll50`, an instantiated PLL component we will create later on in this tutorial.
* `paths`: a list of clock paths, details of which will follow soon.
* `reset`: a tuple of: the name of the _active-high_ port by which we can (asynchronously) reset the clock source, and the expression controlling this reset.
  Now, I lied a bit that we didn't have to write any VHDL or SystemVerilog.
  As you can see, we connect the reset port `areset` to the expression `not KEY0(0)`, where the latter is obviously a VHDL expression.
  Remember that our extra inputs are bit _vectors_, but the `altpll50`s `areset` port expects a single bit.
  Hence we need to select the first (and only) bit: `KEY0(0)`.
  Additionally, remember that the keys are `0` when they are pressed, but the reset port expects an _active-high_ signal.
  Hence we need to negate the signal: `not KEY0(0)`.
* `lock`: the name of the port that the clock signals are stable. In our case it is called `locked`.

Finally, we arrive at the definition of the clock `Path`:

* `inp`: a tuple of: the name port to which the external clock source must be connected, and the name of the external source.
  In our case, the name of the port is `inclk0`.
  Again, we have to write some VHDL.
  Just as for the reset port, the `inclk0` port is expecting a single bit, but our `CLOCK_50` port is a 1-bit _vector_.
  We hence need to extract the first (and only) element: `CLOCK_50(0)`.
* `outp`: a list of tuples, where every tuple consists of the name of an output port of the clock generator, and the clock to which this port should be connected.
  In our case, we only have a single output port, `c0`, which we connect to the `System` clock.

## VHDL generation

Now it's time to generate some VHDL, yay!
Make sure that the `Blinker.hs` and `blinker.topentity` file are in your current working directory, and then run the CλaSH compiler to generate VHDL:

```
clash --vhdl Blinker.hs
```

This will create a `./vhdl/Blinker/` directory, of which, for this tutorial, the most interesting file is within that directory is:

`blinker.vhdl`:
{% highlight vhdl %}
-- Automatically generated VHDL
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use work.all;
use work.types.all;

entity blinker is
  port(KEY1     : in std_logic_vector(0 downto 0);
       CLOCK_50 : in std_logic_vector(0 downto 0);
       KEY0     : in std_logic_vector(0 downto 0);
       LED      : out std_logic_vector(7 downto 0));
end;

architecture structural of blinker is
  signal system1000      : std_logic;
  signal system1000_rstn : std_logic;
  signal altpll50_locked : std_logic;
begin
  altpll50_inst : entity altpll50
    port map
      (inclk0 => CLOCK_50(0)
      ,c0     => system1000
      ,areset => not KEY0(0)
      ,locked => altpll50_locked);

  -- reset system1000_rstn is asynchronously asserted, but synchronously de-asserted
  resetSync_n_0 : block
    signal n_1 : std_logic;
    signal n_2 : std_logic;
  begin
    process(system1000,altpll50_locked)
    begin
      if altpll50_locked = '0' then
        n_1 <= '0';
        n_2 <= '0';
      elsif rising_edge(system1000) then
        n_1 <= '1';
        n_2 <= n_1;
      end if;
    end process;

    system1000_rstn <= n_2;
  end block;

  topEntity_0_inst : entity topEntity_0
    port map
      (key1_i1         => KEY1
      ,system1000      => system1000
      ,system1000_rstn => system1000_rstn
      ,topLet_o        => LED);
end;
{% endhighlight %}

## Port/Pin mappings.

It's almost time to start programming our FPGA.
But first we need to setup how the component ports are connected to the right pins on the FPGA.
For this we will use the `DE0_Nano_SystemBuilder` which you can find on the [DE0-Nano CD-ROM](http://www.terasic.com.tw/cgi-bin/page/archive.pl?Language=English&CategoryNo=165&No=593&PartNo=4).
This will create both a pin/port map file, and a [Quartus II](http://dl.altera.com/?edition=web) project file.
Note that I work on OS X, so both `DE0_Nano_SystemBuilder` and `Quartus II` are running inside a [VirtualBox](https://www.virtualbox.org/) VM running Windows 8.

![SystemBuilder](/assets/images/DE0-NanoSystemBuilder.jpg)

In `DE0_Nano_SystemBuilder`, call the project `blinker`, and ensure that _only_ `CLOCK`, `Button x 2`, and `LED x 8` are selected.
Then press the `Generate` button, this will open a `save` dialog window: select a location where you want to create the project and hit the `save` button.
You can now close `DE0_Nano_SystemBuilder`.

In the directory to which you saved the project you will find the files:

* `blinker.htm`
* `blinker.qpf`
* `blinker.qsf`
* `blinker.sdc`
* `blinker.v`

You can delete `blinker.v` as we will be using our `blinker.vhdl`.
The port/pin mappings that are generated by `DE0_Nano_SystemBuilder` do not completely correspond to our port names.
We will hence need to update the mapping file `blinker.qsf`:

* Replace every occurrence of `CLOCK_50` by `CLOCK_50[0]`
* Replace every occurrence of `KEY[0]` by `KEY0[0]`
* Replace every occurrence of `KEY[1]` by `KEY1[0]`

The final `blinker.qsf` file will look like:

```
#============================================================
# Build by Terasic System Builder
#============================================================

set_global_assignment -name FAMILY "Cyclone IV E"
set_global_assignment -name DEVICE EP4CE22F17C6
set_global_assignment -name TOP_LEVEL_ENTITY "blinker"
set_global_assignment -name ORIGINAL_QUARTUS_VERSION "12.0"
set_global_assignment -name LAST_QUARTUS_VERSION "12.0"
set_global_assignment -name PROJECT_CREATION_TIME_DATE "12:43:02 MAY 01,2015"
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
set_location_assignment PIN_J15 -to KEY0[0]
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to KEY0[0]
set_location_assignment PIN_E1 -to KEY1[0]
set_instance_assignment -name IO_STANDARD "3.3-V LVTTL" -to KEY1[0]

#============================================================
# End of pin assignments by Terasic System Builder
#============================================================
```

Also, since we will be using the `PLL` for clocks, you can replace the entire contents of `blinker.sdc` by:

```
derive_pll_clocks
```

## Quartus II

We now open the `blinker.qpf` file in `Quartus II`.
For this tutorial, I will be using version `14.1` of the `Quartus II` software.
The first thing we will do is add the VHDL files generated by CλaSH to our design.
In the menu bar, go to, `Project -> Add/Remove Files in Project...`, press the `...` button next to `File name`, in the load dialog, select all the files generated by clash, and press `Open`.
Then press `OK` to close the `Settings` dialog.

![BlinkerFiles](/assets/images/quartus2-blinker-files.jpg)

Next we will create a `PLL` component.
On the right of the `Quartus II` main window you will see the `IP Catalog`.
Go to `Installed IP > Library > Basic Functions > Clocks; PLLs and Resets > PLL`, and double click on `ALTPLL`.

![CatalogPLL](/assets/images/quartus2-ipcatalog-altpll.jpg)

This opens a dialog called `Save IP Variation`, at the end of the line enter `altpll50` and press `OK`.

![VariationPLL](/assets/images/quartus2-ipvariation-altpll50.jpg)

This opens the `MegaWizzard` dialog for the PLL.
Change `What is the frequency of the inclk0 input?` to `50.000`.

![WizardPLL](/assets/images/megawizzard-altpll50.jpg)

Then press `Finish` _twice_.
This closes the `MegaWizzard` dialog, and opens a new dialog asking if you want to add this IP block to your project.
We want his, so select 'Yes'.

### Synthesis time

Finally, we're finished with configuration, and we can start creating the configuration file for the FPGA.
In the menu bar, click: 'Processing -> Start Compilation'.
In my extremely slow VM this compilation/synthesis process will take a while, if but might be much faster on your machine.

### Programming time

After synthesis has finished, it is time to program our FPGA board.
Connect the FPGA board to a USB port, and start the programmer from the menu bar: `Tools -> Programmer`.
Press the `Start` button on the left to program your FPGA and wait until the progress bar says `100% (Succesful)`.

![Programmer](/assets/images/quartus2-programmer-blinker.jpg)

## Time to play

Your FPGA should now be fully configured with the CλaSH generated design for you to play with.
So yeah, go press those buttons!

This ends the tutorial on the use of `.topentity` configurations to program your FPGA devices with CλaSH designs.
I hope you got the gist of it.
If/When I get my hands on a Xilinx FPGA board, I hope to make an alternate version of this tutorial, but then use Xilinx' ISE/Vivado tool-flow instead.
