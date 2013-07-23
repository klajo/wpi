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
    wpi:setup(), % or setup_gpio/0 or setup_sys/0
    wpi:pin_mode(Pin, output),
    wpi:digital_write(Pin, 1),

It helps setting the ERL_LIBS environment variable to tell Erlang
where to find the code. Assuming you have a copy of the `wpi`
application in `/home/pi/src/wpi` you can add `wpi` and other apps in
the `src` directory to the code path by setting the environment
variable like this:

    export ERL_LIBS=/home/src/

If you get an error like this, it means that the library doesn't have
root access and you have called `wpi:setup/0` or `wpi:setup_gpio/0`.
This is solved by running Erlang as root, but please consider the
security implications of doing so, or by using the `gpio` binary
followed by `wpi:setup_sys/0` which can be run as a normal user.

    wiringPi: Must be root to call wiringPiSetup(). (Did you forget sudo?)

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

Contributions
-------------

Contributions are more than welcome.  Please commit them on a side
branch (not master) with a descriptive name to make code reviews easier.

Credits
-------

Credits go to Gordon Henderson for the WiringPi library.

[1]: http://wiringpi.com/
[2]: http://wiringpi.com/download-and-install/
[3]: https://github.com/basho/rebar/
[4]: http://wiringpi.com/pins/
