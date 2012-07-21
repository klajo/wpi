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

-ifndef(WPI_HRL).
-define(WPI_HRL, true).

-define(WPI_INPUT, 0).
-define(WPI_OUTPUT, 1).
-define(WPI_PWM_OUTPUT, 2).

-define(WPI_LOW, 0).
-define(WPI_HIGH, 1).

-define(WPI_PUD_OFF, 0).
-define(WPI_PUD_DOWN, 1).
-define(WPI_PUD_UP, 2).

-endif. % WPI_HRL
