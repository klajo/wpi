wpi - an Erlang NIF for the WiringPi library for the Raspberry Pi
=================================================================

This application is an Erlang wrapper around the [WiringPi][1] library
which is a Raspberry Pi dialect of the Wiring library for Arduino. wpi
makes it possible to read from and write to GPIO pins, write to LCDs,
shift bits in and out or control other devices over serial interfaces
or SPI and all this from a Raspberry Pi running Erlang.

Getting started
---------------

Start by [downloading and installing the WiringPi][2] library.

Then it should be possible to build wpi using [rebar][3].

    rebar compile

Before starting, you need to include a hrl file which defines some
useful constants (matches those of WiringPi):

    -include_lib("wpi/include/wpi.hrl").

Then you can start setting up the [pins][4] and use them:

    Pin = 7,
    wpi:pin_mode(Pin, output),
    wpi:digital_write(Pin, 1),

It helps setting the ERL_LIBS environment variable to tell Erlang
where to find the code. Assuming you have a copy of the `wpi`
application in `/home/pi/src/wpi` you can add `wpi` and other apps in
the `src` directory to the code path by setting the environment
variable like this:

    export ERL_LIBS=/home/src/

If you get an error like this, it means that the library doesn't have
root access. This is solved by running Erlang as root, but please
consider the security implications of doing so. A future version of
the library may have another way of handling this.

    wiringPiSetup: Unable to open /dev/mem: Permission denied

Functionality
-----------------------

* read from, write to and control pins
* control and write to LCDs
* shift in/out bits (untested)
* soft PWM
* read from and write to a serial console
* write and read binary data from the SPI bus

Caveats
-------

This NIF is entirely experimental - use at your own risk.  It has been
used to both write to (LED), read from (button) pins and control an
LCD successfully, while some functionality may be untested.

This library currently only supports wiringPiSetup(), not
wiringPiSetupGpio() nor wiringPiSetupSys(). This means that it's
currently only possible to use the WiringPi pin numbering scheme
outlined in the [pins][4] section.

Contributions
-------------

Contributions are more than welcome.

Credits
-------

Credits go to Gordon Henderson for the WiringPi library.

[1]: http://wiringpi.com/
[2]: http://wiringpi.com/download-and-install/
[3]: https://github.com/basho/rebar/
[4]: http://wiringpi.com/pins/
