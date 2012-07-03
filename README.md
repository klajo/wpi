wpi - an Erlang NIF for the WiringPi library for the Raspberry Pi
=================================================================

This application is an Erlang wrapper around the [WiringPi][1] library.

Getting started
---------------

Start by [downloading and installing the WiringPi][2] library.

Then it should be possible to build wpi using [rebar][3].

    rebar compile

Before starting, you need to include a hrl file which defines some
useful constants (matches those of WiringPi):

    -include_lib("wpi/include/wpi.hrl").

Then you can start setting up the pins and use them:

    Pin = 4,
    wpi:gpio_mode(?WPI_MODE_GPIO),
    wpi:pin_mode(Pin, ?WPI_OUTPUT),
    wpi:digital_write(Pin, ?WPI_HIGH),

Caveats
-------

This NIF is entirely experimental - use at your own risk.  It has been
used to both write to (LED) and read from (button) pins successfully,
while some functionality may be untested.

Contributions
-------------

Contributions are more than welcome.

Credits
-------

Credits go to Gordon Henderson for the WiringPi library.

[1]: https://projects.drogon.net/raspberry-pi/wiringpi/
[2]: https://projects.drogon.net/raspberry-pi/wiringpi/download-and-install/
[3]: https://github.com/basho/rebar/