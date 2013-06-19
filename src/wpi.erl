%%% An Erlang NIF which wraps the WiringPi library (by Gordon Henderson).
%%% Copyright (C) 2012  Klas Johansson
%%%
%%% This file is part of wpi.
%%%
%%% wpi is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as
%%% published by the Free Software Foundation, either version 3 of the
%%% License, or (at your option) any later version.
%%%
%%% wpi is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public License
%%% along with wpi.  If not, see <http://www.gnu.org/licenses/>.

%%% @doc
%%% An Erlang NIF for the WiringPi library for the Raspberry Pi
%%%
%%% This application is an Erlang wrapper around the
%%% [http://wiringpi.com/ WiringPi] library which is a Raspberry Pi
%%% dialect of the Wiring library for Arduino. wpi makes it possible
%%% to read from and write to GPIO pins, write to LCDs, shift bits in
%%% and out or control other devices over serial interfaces or SPI and
%%% all this from a Raspberry Pi running Erlang.
%%%
%%% The functions in this API follows the WiringPi
%%% [http://wiringpi.com/pins/ pin numbering scheme].

-module(wpi).

-include_lib("wpi/include/wpi.hrl").

%% setup
-export([setup/0]).
-export([setup_gpio/0]).
-export([setup_sys/0]).

%% the basics: pins and stuff
-export([pin_mode/2]).
-export([digital_write/2]).
-export([pwm_write/2]).
-export([digital_read/1]).
-export([pull_up_dn_control/2]).

%% LCD
-export([lcd_init/8, lcd_init/12]).
-export([lcd_home/1]).
-export([lcd_clear/1]).
-export([lcd_position/3]).
-export([lcd_put_char/2]).
-export([lcd_puts/2]).
-export([lcd_printf/3]).
-export([lcd_format/3]).

%% shift
-export([shift_in/3]).
-export([shift_out/4]).

%% soft PWM
-export([soft_pwm_create/3]).
-export([soft_pwm_write/2]).

%% serial
-export([serial_open/2]).
-export([serial_close/1]).
-export([serial_flush/1]).
-export([serial_put_char/2]).
-export([serial_puts/2]).
-export([serial_printf/3]).
-export([serial_format/3]).
-export([serial_data_avail/1]).
-export([serial_get_char/1]).

%% SPI
-export([spi_get_fd/1]).
-export([spi_data_rw/2]).
-export([spi_setup/2]).

-define(nif_stub,
        erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE})).

-on_load(on_load/0).

-type wpi_pin_mode()       :: 0..2     % WPI_INPUT | WPI_OUTPUT | WPI_PWM_OUTPUT
                            | input | output | pwm_output.
-type wpi_pin_number()     :: integer().
-type wpi_digital_value()  :: 0..1.    % WPI_LOW | WPI_HIGH
-type wpi_pwm_value()      :: 0..1023.
-type wpi_pud_mode()       :: 0..2     % WPI_PUD_OFF | WPI_PUD_DOWN | WPI_PUD_UP
                            | off | down | up.
-opaque wpi_lcd_handle()   :: integer().
-type wpi_bit_order()      :: 0..1     % WPI_LSB_FIRST | WPI_MSB_FIRST
                            | lsb_first | msb_first.
-type wpi_uint8()          :: 0..255.

-opaque wpi_serial_handle() :: integer().

-type wpi_baud()            :: integer().

-type wpi_spi_channel()     :: 0..1.


on_load() ->
    %% Force wiringPi to return -1 on initalization failure. In
    %% wiringPi this was not necessary, but starting at v2 the setup
    %% functions exit instead.  a change between wiringPi v1 and v2.
    %% Use return values in order to provide more erlangy error handling.
    os:putenv("WIRINGPI_CODES", "true"),
    ok = erlang:load_nif(filename:join(code:priv_dir(wpi), "./wpi_drv"), 0).

-spec(setup() -> ok | {error, term()}).
%% @doc This initialises wiringPi and assumes that the calling program
%% is going to be using the wiringPi pin numbering scheme. This is a
%% simplified numbering scheme which provides a mapping from virtual
%% pin numbers 0 through 16 to the real underlying Broadcom GPIO pin
%% numbers. See the pins page on the wiringPi web page for a table
%% which maps the wiringPi pin number to the Broadcom GPIO pin number
%% to the physical location on the edge connector.
%%
%% This function needs to be called with root privileges.
setup() ->
    setup_nif().

-spec(setup_gpio() -> ok | {error, term()}).
%% @doc This is identical to {@link setup/0}, however it allows the
%% calling programs to use the Broadcom GPIO pin numbers directly with
%% no re-mapping.
%%
%% As for {@link setup/0}, this function needs to be called with root
%% privileges, and note that some pins are different from revision 1
%% to revision 2 boards.
setup_gpio() ->
    setup_gpio_nif().

-spec(setup_sys() -> ok | {error, term()}).
%% @doc This initialises wiringPi but uses the /sys/class/gpio
%% interface rather than accessing the hardware directly. This can be
%% called as a non-root user provided the GPIO pins have been exported
%% before-hand using the gpio program. Pin numbering in this mode is
%% the native Broadcom GPIO numbers - the same as {@link setup_gpio/0}
%% above, so be aware of the differences between Rev 1 and Rev 2
%% boards.
%%
%% Note: In this mode you can only use the pins which have been
%% exported via the /sys/class/gpio interface before you run your
%% program. You can do this in a separate shell-script, or by using
%% the {@link wpi_gpio} module.
%%
%% Also note that some functions have no effect when using this mode
%% as they're not currently possible to action unless called with root
%% privileges (although you can use {@link wpi_gpio} to set/change
%% modes if needed).
setup_sys() ->
    setup_sys_nif().

setup_nif()      -> ?nif_stub.
setup_gpio_nif() -> ?nif_stub.
setup_sys_nif()  -> ?nif_stub.

%% The basics: pins and stuff
-spec pin_mode(wpi_pin_number(), wpi_pin_mode()) -> ok.
%% @doc Set the mode of a pin to either input, output, or PWM
%% output. Note that only WiringPi pin 1 (GPIO 18) supports PWM output.
%% @see wpi:soft_pwm_write/2
pin_mode(Pin, input) ->
    pin_mode(Pin, ?WPI_INPUT);
pin_mode(Pin, output) ->
    pin_mode(Pin, ?WPI_OUTPUT);
pin_mode(Pin, pwm_output) ->
    pin_mode(Pin, ?WPI_PWM_OUTPUT);
pin_mode(Pin, Mode) when is_integer(Pin),
                         (Mode == ?WPI_INPUT orelse
                          Mode == ?WPI_OUTPUT orelse
                          Mode == ?WPI_PWM_OUTPUT) ->
    pin_mode_nif(Pin, Mode).

-spec digital_write(wpi_pin_number(), wpi_digital_value()) -> ok.
%% @doc Write the value high or low (1 or 0) to the given pin which
%% must have been previously set as an output.
digital_write(Pin, Value) when is_integer(Pin),
                               (Value == ?WPI_LOW orelse
                                Value == ?WPI_HIGH) ->
    digital_write_nif(Pin, Value).

-spec pwm_write(wpi_pin_number(), wpi_pwm_value()) -> ok.
%% @doc Write the value to the PWM register for the given pin. The
%% value must be between 0 and 1024. (Again, note that only pin 1
%% supports PWM).
%% @see wpi:soft_pwm_write/2
pwm_write(Pin, Value) when is_integer(Pin),
                           is_integer(Value),
                           Value >= 0,
                           Value < 1024 ->
    pwm_write_nif(Pin, Value).

-spec digital_read(wpi_pin_number()) -> wpi_digital_value().
%% @doc Return the value read from the given pin. It will be high or low
%% (1 or 0) depending on the logic level at the pin.
digital_read(Pin) when is_integer(Pin) ->
    digital_read_nif(Pin).

-spec pull_up_dn_control(wpi_pin_number(), wpi_pud_mode()) -> ok.
%% @doc Set the pull-up or pull-down resistor mode on the given pin,
%% which should be set as an input. Unlike the Arduino, the BCM2835
%% has both pull-up an down internal resistors. The parameter pud
%% should be; `off' (no pull up/down), `down' (pull to ground) or
%% `up' (pull to 3.3v)
pull_up_dn_control(Pin, off) ->
    pull_up_dn_control(Pin, ?WPI_PUD_OFF);
pull_up_dn_control(Pin, down) ->
    pull_up_dn_control(Pin, ?WPI_PUD_DOWN);
pull_up_dn_control(Pin, up) ->
    pull_up_dn_control(Pin, ?WPI_PUD_UP);
pull_up_dn_control(Pin, Mode) when is_integer(Pin),
                                   (Mode == ?WPI_PUD_OFF orelse
                                    Mode == ?WPI_PUD_DOWN orelse
                                    Mode == ?WPI_PUD_UP) ->
    pull_up_dn_control_nif(Pin, Mode).

pin_mode_nif(_Pin, _Mode)           -> ?nif_stub.
digital_write_nif(_Pin, _Value)     -> ?nif_stub.
pwm_write_nif(_Pin, _Value)         -> ?nif_stub.
digital_read_nif(_Pin)              -> ?nif_stub.
pull_up_dn_control_nif(_Pin, _Mode) -> ?nif_stub.

%% LCD
-spec lcd_init(integer(), integer(),
               wpi_pin_number(), wpi_pin_number(),
               wpi_pin_number(), wpi_pin_number(),
               wpi_pin_number(), wpi_pin_number()) -> wpi_lcd_handle().
%% @doc Initialize an LCD in 4-bit mode.
%% @see wpi:lcd_init/12
lcd_init(NumRows, NumCols, RsPin, EPin, D0Pin, D1Pin, D2Pin, D3Pin)
  when is_integer(NumRows), NumRows > 0, is_integer(NumCols), NumCols > 0,
       is_integer(RsPin), is_integer(EPin),
       is_integer(D0Pin), is_integer(D1Pin),
       is_integer(D2Pin), is_integer(D3Pin) ->
    lcd_init_nif(NumRows, NumCols, _NumBits=4, RsPin, EPin,
                 D0Pin, D1Pin, D2Pin, D3Pin, 0, 0, 0, 0).

-spec lcd_init(integer(), integer(),
               wpi_pin_number(), wpi_pin_number(),
               wpi_pin_number(), wpi_pin_number(),
               wpi_pin_number(), wpi_pin_number(),
               wpi_pin_number(), wpi_pin_number(),
               wpi_pin_number(), wpi_pin_number()) -> wpi_lcd_handle().
%% @doc Initialize an LCD in 8-bit mode.
%%
%% NumRows and NumCols are the rows and columns on the display
%% (e.g. 2, 16 or 4, 20). The RsPin and EPin represent the pin numbers
%% of the display's RS pin and Strobe (E) pin. The parameters D0Pin
%% through D7Pin are the pin numbers of the 8 data pins connected from
%% the Pi to the display. There are two versions of this function, one
%% which shall be used if the display is used in 4-bit mode, the other
%% in 8-bit mode.
%%
%% The return value is the handle to be used for all subsequent
%% calls to the LCD functions when dealing with that LCD, or -1 to
%% indicate a fault (usually incorrect parameters).
lcd_init(NumRows, NumCols, RsPin, EPin,
         D0Pin, D1Pin, D2Pin, D3Pin, D4Pin, D5Pin, D6Pin, D7Pin)
  when is_integer(NumRows), NumRows > 0, is_integer(NumCols), NumCols > 0,
       is_integer(RsPin), is_integer(EPin),
       is_integer(D0Pin), is_integer(D1Pin),
       is_integer(D2Pin), is_integer(D3Pin),
       is_integer(D4Pin), is_integer(D5Pin),
       is_integer(D6Pin), is_integer(D7Pin) ->
    lcd_init_nif(NumRows, NumCols, _NumBits=8, RsPin, EPin,
                 D0Pin, D1Pin, D2Pin, D3Pin, D4Pin, D5Pin, D6Pin, D7Pin).

-spec lcd_home(wpi_lcd_handle()) -> ok.
%% @doc Set the cursor to the home (upper left corner)
%% position of the LCD.
lcd_home(Handle) when is_integer(Handle) ->
    lcd_home_nif(Handle).

-spec lcd_clear(wpi_lcd_handle()) -> ok.
%% @doc Clear the contents of the LCD.
lcd_clear(Handle) when is_integer(Handle) ->
    lcd_clear_nif(Handle).

-spec lcd_position(wpi_lcd_handle(), integer(), integer()) -> ok.
%% @doc Set the cursor position for subsequent text entry. The upper
%% left corner is 0, 0.
lcd_position(Handle, X, Y)
  when is_integer(Handle), is_integer(X), X >= 0, is_integer(Y), Y >= 0  ->
    lcd_position_nif(Handle, X, Y).

-spec lcd_put_char(wpi_lcd_handle(), 0..255) -> ok.
%% @doc Write a single ASCII character to the LCD.
lcd_put_char(Handle, Char)
  when is_integer(Handle), is_integer(Char), Char >= 0, Char =< 255  ->
    lcd_put_char_nif(Handle, Char).

-spec lcd_puts(wpi_lcd_handle(), string()) -> ok.
%% @doc Write a string to the LCD.
lcd_puts(Handle, String)
  when is_integer(Handle), is_list(String) ->
    lcd_puts_nif(Handle, length(String), String).

-spec lcd_printf(wpi_lcd_handle(), string(), list(any())) -> ok.
%% @doc Not supported.
%% @see wpi:lcd_format/3
lcd_printf(_Handle, _Format, _Args) ->
    erlang:error(not_supported).

-spec lcd_format(wpi_lcd_handle(), string(), list(any())) -> ok.
%% @doc Format data and write to the LCD. This is a more erlangy
%% version of lcd_printf/3 and follows the same formatting rules as io:format/2.
lcd_format(Handle, Format, Args)
  when is_integer(Handle), is_list(Format), is_list(Args) ->
    lcd_puts(Handle, lists:flatten(io_lib:format(Format, Args))).

lcd_init_nif(_NumRows, _NumCols, _NumBits, _RsPin, _EPin,
             _D0Pin, _D1Pin, _D2Pin, _D3Pin, _D4Pin, _D5Pin, _D6Pin, _D7Pin) ->
    ?nif_stub.
lcd_home_nif(_Handle)                      -> ?nif_stub.
lcd_clear_nif(_Handle)                     -> ?nif_stub.
lcd_position_nif(_Handle, _X, _Y)          -> ?nif_stub.
lcd_put_char_nif(_Handle, _Char)           -> ?nif_stub.
lcd_puts_nif(_Handle, _StringLen, _String) -> ?nif_stub.

-spec shift_in(wpi_pin_number(), wpi_pin_number(), wpi_bit_order()) ->
                      wpi_uint8().
%% @doc Shift an 8-bit data value in with the data appearing on the
%% DataPin and the clock being sent out on the ClockPin. Order is either
%% `lsb_first' or `msb_first'. The data is sampled after the ClockPin goes
%% high. (So ClockPin high, sample data, ClockPin low, repeat for 8 bits) The
%% 8-bit value is returned by the function.
shift_in(DataPin, ClockPin, lsb_first) ->
    shift_in(DataPin, ClockPin, ?WPI_LSB_FIRST);
shift_in(DataPin, ClockPin, msb_first) ->
    shift_in(DataPin, ClockPin, ?WPI_MSB_FIRST);
shift_in(DataPin, ClockPin, Order)
  when is_integer(DataPin), is_integer(ClockPin), is_integer(Order) ->
    shift_in_nif(DataPin, ClockPin, Order).

-spec shift_out(wpi_pin_number(), wpi_pin_number(), wpi_bit_order(),
                wpi_uint8()) -> ok.
%% @doc Shift an 8-bit data value out with the data being sent out on
%% DataPin and the clock being sent out on the ClockPin. Order is
%% either `lsb_first' or `msb_first'. Data is clocked out on the
%% rising or falling edge - ie. DataPin is set, then ClockPin is taken
%% high then low - repeated for the 8 bits.
shift_out(DataPin, ClockPin, lsb_first, Value) ->
    shift_out(DataPin, ClockPin, ?WPI_LSB_FIRST, Value);
shift_out(DataPin, ClockPin, msb_first, Value) ->
    shift_out(DataPin, ClockPin, ?WPI_MSB_FIRST, Value);
shift_out(DataPin, ClockPin, Order, Value)
  when is_integer(DataPin), is_integer(ClockPin), is_integer(Order),
       is_integer(Value) ->
    shift_out_nif(DataPin, ClockPin, Order, Value).

shift_in_nif(_DataPin, _ClockPin, _Order)          -> ?nif_stub.
shift_out_nif(_DataPin, _ClockPin, _Order, _Value) -> ?nif_stub.

-spec soft_pwm_create(wpi_pin_number(), integer(), integer()) -> ok.
%% @doc Create a software controlled PWM pin. You can use any GPIO
%% pin. If the Range is 100, then the value can be anything from 0
%% (off) to 100 (fully on) for the given pin.
soft_pwm_create(Pin, InitValue, Range) when is_integer(Pin),
                                            is_integer(InitValue),
                                            is_integer(Range) ->
    soft_pwm_create_nif(Pin, InitValue, Range).

-spec soft_pwm_write(wpi_pin_number(), integer()) -> ok.
%% @doc Update the PWM value on the given pin. The value is checked to
%% be in-range and pins that haven't previously been initialised via
%% soft_pwm_create will be silently ignored.
soft_pwm_write(Pin, Value) when is_integer(Pin), is_integer(Value) ->
    soft_pwm_write_nif(Pin, Value).

soft_pwm_create_nif(_Pin, _InitValue, _Range) -> ?nif_stub.
soft_pwm_write_nif(_Pin, _Value)              -> ?nif_stub.

%% serial
-spec serial_open(string(), wpi_baud()) -> ok.
%% @doc Open and initialize the serial device and set the baud
%% rate. It sets the port into raw mode (character at a time and no
%% translations), and sets the read timeout to 10 seconds. The return
%% value is the file descriptor or -1 for any error, in which case
%% errno will be set as appropriate.
serial_open(Device, Baud) when is_list(Device), is_integer(Baud) ->
    serial_open_nif(Baud, length(Device), Device).

-spec serial_close(wpi_serial_handle()) -> ok.
%% @doc Close the device identified by the given handle.
serial_close(Handle) when is_integer(Handle) ->
    serial_close_nif(Handle).

-spec serial_flush(wpi_serial_handle()) -> ok.
%% @doc Discard all data received, or waiting to be sent to the given device.
serial_flush(Handle) when is_integer(Handle) ->
    serial_flush_nif(Handle).

-spec serial_put_char(wpi_serial_handle(), 0..255) -> ok.
%% @doc Send the single byte to the serial device identified by the
%% given handle.
serial_put_char(Handle, Char)
  when is_integer(Handle), is_integer(Char), Char >= 0, Char =< 255  ->
    serial_put_char_nif(Handle, Char).

-spec serial_puts(wpi_serial_handle(), string()) -> ok.
%% @doc Send the string to the serial device identified by the given handle.
serial_puts(Handle, String)
  when is_integer(Handle), is_list(String) ->
    serial_puts_nif(Handle, length(String), String).

-spec serial_printf(wpi_serial_handle(), string(), list(any())) -> ok.
%% @doc Not supported.
%% @see wpi:serial_format/3
serial_printf(_Handle, _Format, _Args) ->
    erlang:error(not_supported).

-spec serial_format(wpi_serial_handle(), string(), list(any())) -> ok.
%% @doc Format data and write to the serial device identified by the
%% given handle. This is a more erlangy version of lcd_printf/3 and
%% follows the same formatting rules as io:format/2.
serial_format(Handle, Format, Args)
  when is_integer(Handle), is_list(Format), is_list(Args) ->
    serial_puts(Handle, lists:flatten(io_lib:format(Format, Args))).

-spec serial_data_avail(wpi_serial_handle()) -> integer().
%% @doc Return the number of characters available for reading.
serial_data_avail(Handle) when is_integer(Handle) ->
    serial_data_avail_nif(Handle).

-spec serial_get_char(wpi_serial_handle()) -> 0..255.
%% @doc Return the next character available on the serial device.
%%
%% Warning: This call will block for up to 10 seconds if no data is
%% available. This has the effect that the emulator will be blocked
%% (for up to 10s!) and no other process may run (unless there are
%% more cores/schedulers available). From wiringSerial.c:
%% ```
%%    options.c_cc [VTIME] = 100 ;        // Ten seconds (100 deciseconds)
%% '''
%% Todo: Fix the above warning.
serial_get_char(Handle) when is_integer(Handle)  ->
    serial_get_char_nif(Handle).

serial_open_nif(_Baud, _StringLen, _Device)   -> ?nif_stub.
serial_close_nif(_Handle)                     -> ?nif_stub.
serial_flush_nif(_Handle)                     -> ?nif_stub.
serial_put_char_nif(_Handle, _Char)           -> ?nif_stub.
serial_puts_nif(_Handle, _StringLen, _String) -> ?nif_stub.
serial_data_avail_nif(_Handle)                -> ?nif_stub.
serial_get_char_nif(_Handle)                  -> ?nif_stub.

%% SPI
-spec spi_get_fd(wpi_spi_channel()) -> integer().
%% @doc Return the file-descriptor for the given SPI channel.
spi_get_fd(Channel) when (Channel == 0 orelse Channel == 1) ->
    spi_get_fd_nif(Channel).

-spec spi_data_rw(wpi_spi_channel(), binary()) ->
                  {ok, binary()} |
                  {error, {failed_to_read_write_data, integer()}}.
%% @doc Write and read a block of data over the SPI bus.
spi_data_rw(Channel, WriteData) when (Channel == 0 orelse Channel == 1),
                                     is_binary(WriteData) ->
    spi_data_rw_nif(Channel, WriteData, byte_size(WriteData)).

-spec spi_setup(wpi_spi_channel(), integer()) -> integer().
%% @doc Open and set up the SPI device. See Gordon's
%% [https://projects.drogon.net/understanding-spi-on-the-raspberry-pi/
%% Understanding SPI on the Raspberry Pi] for more information.
spi_setup(Channel, Speed) when (Channel == 0 orelse Channel == 1),
                               is_integer(Speed), Speed > 0 ->
    spi_setup_nif(Channel, Speed).

spi_get_fd_nif(_Channel)                    -> ?nif_stub.
spi_data_rw_nif(_Channel, _WriteData, _Len) -> ?nif_stub.
spi_setup_nif(_Channel, _Speed)             -> ?nif_stub.
