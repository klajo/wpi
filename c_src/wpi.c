// An Erlang NIF which wraps the WiringPi library (by Gordon Henderson).
// Copyright (C) 2012  Klas Johansson
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "erl_nif.h"
#include <wiringPi.h>

static int
load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    return wiringPiSetup(); // returns -1 in case of error ==> loading fails
}

static ERL_NIF_TERM
gpio_mode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int mode;
    if (!enif_get_int(env, argv[0], &mode))
    {
        return enif_make_badarg(env);
    }
    wiringPiGpioMode(mode);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
pin_mode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int pin, mode;
    if (!enif_get_int(env, argv[0], &pin))
    {
        return enif_make_badarg(env);
    }
    if (!enif_get_int(env, argv[1], &mode))
    {
        return enif_make_badarg(env);
    }
    pinMode(pin, mode);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
digital_write(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int pin, value;
    if (!enif_get_int(env, argv[0], &pin))
    {
        return enif_make_badarg(env);
    }
    if (!enif_get_int(env, argv[1], &value))
    {
        return enif_make_badarg(env);
    }
    digitalWrite(pin, value);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
pwm_write(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int pin, value;
    if (!enif_get_int(env, argv[0], &pin))
    {
        return enif_make_badarg(env);
    }
    if (!enif_get_int(env, argv[1], &value))
    {
        return enif_make_badarg(env);
    }
    pwmWrite(pin, value);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
digital_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int pin, value;
    if (!enif_get_int(env, argv[0], &pin))
    {
        return enif_make_badarg(env);
    }
    value = digitalRead(pin);
    return enif_make_int(env, value);
}

static ERL_NIF_TERM
pull_up_dn_control(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int pin, mode;
    if (!enif_get_int(env, argv[0], &pin))
    {
        return enif_make_badarg(env);
    }
    if (!enif_get_int(env, argv[1], &mode))
    {
        return enif_make_badarg(env);
    }
    pullUpDnControl(pin, mode);
    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] =
    {
        {"gpio_mode",          1, gpio_mode},
        {"pin_mode",           2, pin_mode},
        {"digital_write",      2, digital_write},
        {"pwm_write",          2, pwm_write},
        {"digital_read",       1, digital_read},
        {"pull_up_dn_control", 2, pull_up_dn_control}
    };

ERL_NIF_INIT(wpi, nif_funcs, load, NULL, NULL, NULL)
