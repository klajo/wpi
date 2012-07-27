%%% An Erlang NIF which wraps the WiringPi library (by Gordon Henderson).
%%% Copyright (C) 2012  Klas Johansson
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module(wpi).

-include_lib("wpi/include/wpi.hrl").

-export([pin_mode/2]).
-export([digital_write/2]).
-export([pwm_write/2]).
-export([digital_read/1]).
-export([pull_up_dn_control/2]).

-define(nif_stub,
        erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE})).

-on_load(on_load/0).

-type wpi_pin_mode()      :: 0..27    % WPI_INPUT | WPI_OUTPUT | WPI_PWM_OUTPUT
                             | input | output | pwm_output.
-type wpi_pin_number()    :: integer().
-type wpi_digital_value() :: 0..1.    % WPI_LOW | WPI_HIGH
-type wpi_pwm_value()     :: 0..1023.
-type wpi_pud_mode()      :: 0..2     % WPI_PUD_OFF | WPI_PUD_DOWN | WPI_PUD_UP
                             | off | down | up.

on_load() ->
    ok = erlang:load_nif(filename:join(code:priv_dir(wpi), "./wpi_drv"), 0).

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
