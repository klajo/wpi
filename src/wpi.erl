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

-module(wpi).

-include_lib("wpi/include/wpi.hrl").

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

-define(nif_stub,
        erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE})).

-on_load(on_load/0).

-type wpi_pin_mode()      :: 0..2     % WPI_INPUT | WPI_OUTPUT | WPI_PWM_OUTPUT
                             | input | output | pwm_output.
-type wpi_pin_number()    :: integer().
-type wpi_digital_value() :: 0..1.    % WPI_LOW | WPI_HIGH
-type wpi_pwm_value()     :: 0..1023.
-type wpi_pud_mode()      :: 0..2     % WPI_PUD_OFF | WPI_PUD_DOWN | WPI_PUD_UP
                             | off | down | up.
-opaque wpi_lcd_handle()  :: integer().
-type wpi_bit_order()     :: 0..1     % WPI_LSB_FIRST | WPI_MSB_FIRST
                             | lsb_first | msb_first.
-type wpi_uint8()         :: 0..255.

on_load() ->
    ok = erlang:load_nif(filename:join(code:priv_dir(wpi), "./wpi_drv"), 0).

%% the basics: pins and stuff
-spec pin_mode(wpi_pin_number(), wpi_pin_mode()) -> ok.
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
digital_write(Pin, Value) when is_integer(Pin),
                               (Value == ?WPI_LOW orelse
                                Value == ?WPI_HIGH) ->
    digital_write_nif(Pin, Value).

-spec pwm_write(wpi_pin_number(), wpi_pwm_value()) -> ok.
pwm_write(Pin, Value) when is_integer(Pin),
                           is_integer(Value),
                           Value >= 0,
                           Value < 1024 ->
    pwm_write_nif(Pin, Value).

-spec digital_read(wpi_pin_number()) -> wpi_digital_value().
digital_read(Pin) when is_integer(Pin) ->
    digital_read_nif(Pin).

-spec pull_up_dn_control(wpi_pin_number(), wpi_pud_mode()) -> ok.
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
lcd_home(Handle) when is_integer(Handle) ->
    lcd_home_nif(Handle).

-spec lcd_clear(wpi_lcd_handle()) -> ok.
lcd_clear(Handle) when is_integer(Handle) ->
    lcd_clear_nif(Handle).

-spec lcd_position(wpi_lcd_handle(), integer(), integer()) -> ok.
lcd_position(Handle, X, Y)
  when is_integer(Handle), is_integer(X), X >= 0, is_integer(Y), Y >= 0  ->
    lcd_position_nif(Handle, X, Y).

-spec lcd_put_char(wpi_lcd_handle(), 0..255) -> ok.
lcd_put_char(Handle, Char)
  when is_integer(Handle), is_integer(Char), Char >= 0, Char =< 255  ->
    lcd_put_char_nif(Handle, Char).

-spec lcd_puts(wpi_lcd_handle(), string()) -> ok.
lcd_puts(Handle, String)
  when is_integer(Handle), is_list(String) ->
    lcd_puts_nif(Handle, length(String), String).

-spec lcd_printf(wpi_lcd_handle(), string(), list(any())) -> ok.
lcd_printf(_Handle, _Format, _Args) ->
    erlang:error(not_supported).

-spec lcd_format(wpi_lcd_handle(), string(), list(any())) -> ok.
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
shift_in(DataPin, ClockPin, lsb_first) ->
    shift_in(DataPin, ClockPin, ?WPI_LSB_FIRST);
shift_in(DataPin, ClockPin, msb_first) ->
    shift_in(DataPin, ClockPin, ?WPI_MSB_FIRST);
shift_in(DataPin, ClockPin, Order)
  when is_integer(DataPin), is_integer(ClockPin), is_integer(Order) ->
    shift_in_nif(DataPin, ClockPin, Order).

-spec shift_out(wpi_pin_number(), wpi_pin_number(), wpi_bit_order(),
                wpi_uint8()) -> ok.
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
soft_pwm_create(Pin, InitValue, Range) when is_integer(Pin),
                                            is_integer(InitValue),
                                            is_integer(Range) ->
    soft_pwm_create_nif(Pin, InitValue, Range).

-spec soft_pwm_write(wpi_pin_number(), integer()) -> ok.
soft_pwm_write(Pin, Value) when is_integer(Pin), is_integer(Value) ->
    soft_pwm_write_nif(Pin, Value).

soft_pwm_create_nif(_Pin, _InitValue, _Range) -> ?nif_stub.
soft_pwm_write_nif(_Pin, _Value)              -> ?nif_stub.
