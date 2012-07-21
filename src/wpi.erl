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

-type wpi_pin_mode()      :: 0..2.    % WPI_INPUT|WPI_OUTPUT|WPI_PWM_OUTPUT
-type wpi_pin_number()    :: integer().
-type wpi_digital_value() :: 0..1.    % WPI_LOW|WPI_HIGH
-type wpi_pwm_value()     :: 0..1024.
-type wpi_pud_mode()      :: 0..2.    % WPI_PUD_OFF|WPI_PUD_DOWN|WPI_PUD_DOWN

on_load() ->
    ok = erlang:load_nif(filename:join(code:priv_dir(wpi), "./wpi_drv"), 0).

-spec pin_mode(wpi_pin_number(), wpi_pin_mode()) -> ok.
pin_mode(_Pin, _Mode) ->
    ?nif_stub.

-spec digital_write(wpi_pin_number(), wpi_digital_value()) -> ok.
digital_write(_Pin, _Value) ->
    ?nif_stub.

-spec pwm_write(wpi_pin_number(), wpi_pwm_value()) -> ok.
pwm_write(_Pin, _Value) ->
    ?nif_stub.

-spec digital_read(wpi_pin_number()) -> wpi_digital_value().
digital_read(_Pin) ->
    ?nif_stub.

-spec pull_up_dn_control(wpi_pin_number(), wpi_pud_mode()) -> ok.
pull_up_dn_control(_Pin, _Mode) ->
    ?nif_stub.
