// An Erlang NIF which wraps the WiringPi library (by Gordon Henderson).
// Copyright (C) 2012  Klas Johansson
//
// This file is part of wpi.
//
// wpi is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// wpi is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with wpi.  If not, see <http://www.gnu.org/licenses/>.

#include <stdint.h>

#include "erl_nif.h"
#include <wiringPi.h>
#include <lcd.h>
#include <wiringShift.h>
#include <softPwm.h>
#include <wiringSerial.h>
#include <wiringPiSPI.h>

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;

static int
load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    return wiringPiSetup(); // returns -1 in case of error ==> loading fails
}

static ERL_NIF_TERM
pin_mode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int pin, mode;
    if (!enif_get_int(env, argv[0], &pin) ||
        !enif_get_int(env, argv[1], &mode))
    {
        return enif_make_badarg(env);
    }
    pinMode(pin, mode);
    return atom_ok;
}

static ERL_NIF_TERM
digital_write_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int pin, value;
    if (!enif_get_int(env, argv[0], &pin) ||
        !enif_get_int(env, argv[1], &value))
    {
        return enif_make_badarg(env);
    }
    digitalWrite(pin, value);
    return atom_ok;
}

static ERL_NIF_TERM
pwm_write_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int pin, value;
    if (!enif_get_int(env, argv[0], &pin) ||
        !enif_get_int(env, argv[1], &value))
    {
        return enif_make_badarg(env);
    }
    pwmWrite(pin, value);
    return atom_ok;
}

static ERL_NIF_TERM
digital_read_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
pull_up_dn_control_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int pin, mode;
    if (!enif_get_int(env, argv[0], &pin) ||
        !enif_get_int(env, argv[1], &mode))
    {
        return enif_make_badarg(env);
    }
    pullUpDnControl(pin, mode);
    return atom_ok;
}

static ERL_NIF_TERM
lcd_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int num_rows, num_cols, num_bits, rs_pin, e_pin;
    int d0_pin, d1_pin, d2_pin, d3_pin, d4_pin, d5_pin, d6_pin, d7_pin;
    int handle;
    if (!enif_get_int(env, argv[0],  &num_rows) ||
        !enif_get_int(env, argv[1],  &num_cols) ||
        !enif_get_int(env, argv[2],  &num_bits) ||
        !enif_get_int(env, argv[3],  &rs_pin)   ||
        !enif_get_int(env, argv[4],  &e_pin)    ||
        !enif_get_int(env, argv[5],  &d0_pin)   ||
        !enif_get_int(env, argv[6],  &d1_pin)   ||
        !enif_get_int(env, argv[7],  &d2_pin)   ||
        !enif_get_int(env, argv[8],  &d3_pin)   ||
        !enif_get_int(env, argv[9],  &d4_pin)   ||
        !enif_get_int(env, argv[10], &d5_pin)   ||
        !enif_get_int(env, argv[11], &d6_pin)   ||
        !enif_get_int(env, argv[12], &d7_pin))
    {
        return enif_make_badarg(env);
    }
    handle = lcdInit(num_rows, num_cols, num_bits, rs_pin, e_pin,
                     d0_pin, d1_pin, d2_pin, d3_pin,
                     d4_pin, d5_pin, d6_pin, d7_pin);
    return enif_make_int(env, handle);
}

static ERL_NIF_TERM
lcd_home_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int handle;
    if (!enif_get_int(env, argv[0], &handle))
    {
        return enif_make_badarg(env);
    }
    lcdHome(handle);
    return atom_ok;
}

static ERL_NIF_TERM
lcd_clear_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int handle;
    if (!enif_get_int(env, argv[0], &handle))
    {
        return enif_make_badarg(env);
    }
    lcdClear(handle);
    return atom_ok;
}

static ERL_NIF_TERM
lcd_position_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int handle, x, y;
    if (!enif_get_int(env, argv[0], &handle) ||
        !enif_get_int(env, argv[1], &x)      ||
        !enif_get_int(env, argv[2], &y))
    {
        return enif_make_badarg(env);
    }
    lcdPosition(handle, x, y);
    return atom_ok;
}

static ERL_NIF_TERM
lcd_put_char_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int handle, character;
    if (!enif_get_int(env, argv[0], &handle) ||
        !enif_get_int(env, argv[1], &character))
    {
        return enif_make_badarg(env);
    }
    lcdPutchar(handle, (uint8_t)character);
    return atom_ok;
}

static ERL_NIF_TERM
lcd_puts_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int handle, str_len;
    if (!enif_get_int(env, argv[0], &handle) ||
        !enif_get_int(env, argv[1], &str_len))
    {
        return enif_make_badarg(env);
    }
    char str[str_len+1];
    if (!enif_get_string(env, argv[2], str, sizeof(str), ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }
    lcdPuts(handle, str);
    return atom_ok;
}

static ERL_NIF_TERM
shift_in_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int data_pin, clock_pin, order, value;
    if (!enif_get_int(env, argv[0], &data_pin)  ||
        !enif_get_int(env, argv[1], &clock_pin) ||
        !enif_get_int(env, argv[2], &order))
    {
        return enif_make_badarg(env);
    }
    value = shiftIn((uint8_t)data_pin, (uint8_t)clock_pin, (uint8_t)order);
    return enif_make_int(env, value);
}

static ERL_NIF_TERM
shift_out_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int data_pin, clock_pin, order, value;
    if (!enif_get_int(env, argv[0], &data_pin)  ||
        !enif_get_int(env, argv[1], &clock_pin) ||
        !enif_get_int(env, argv[2], &order)     ||
        !enif_get_int(env, argv[3], &value))
    {
        return enif_make_badarg(env);
    }
    shiftOut((uint8_t)data_pin, (uint8_t)clock_pin,
             (uint8_t)order, (uint8_t)value);
    return atom_ok;
}

// soft PWM
static ERL_NIF_TERM
soft_pwm_create_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int pin, init_value, range, result;
    ERL_NIF_TERM atom_fail, err_code;
    if (!enif_get_int(env, argv[0], &pin)        ||
        !enif_get_int(env, argv[1], &init_value) ||
        !enif_get_int(env, argv[2], &range))
    {
        return enif_make_badarg(env);
    }
    result = softPwmCreate(pin, init_value, range);
    if (result)
    {
        atom_fail = enif_make_atom(env, "failed_to_init_pwm_pin");
        err_code = enif_make_int(env, result);
        return enif_make_tuple2(env,
                                atom_error,
                                enif_make_tuple2(env, atom_fail, err_code));
    }
    else
    {
        return atom_ok;
    }
}

static ERL_NIF_TERM
soft_pwm_write_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int pin, value;
    if (!enif_get_int(env, argv[0], &pin) ||
        !enif_get_int(env, argv[1], &value))
    {
        return enif_make_badarg(env);
    }
    softPwmWrite(pin, value);
    return atom_ok;
}

// serial
static ERL_NIF_TERM
serial_open_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int str_len, baud, handle;
    if (!enif_get_int(env, argv[0], &baud) || 
        !enif_get_int(env, argv[1], &str_len))
    {
        return enif_make_badarg(env);
    }
    char device[str_len+1];
    if (!enif_get_string(env, argv[2], device, sizeof(device), ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }

    handle = serialOpen(device, baud);
    return enif_make_int(env, handle);
}

static ERL_NIF_TERM
serial_close_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int handle;
    if (!enif_get_int(env, argv[0], &handle))
    {
        return enif_make_badarg(env);
    }
    serialClose(handle);
    return atom_ok;
}

static ERL_NIF_TERM
serial_flush_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int handle;
    if (!enif_get_int(env, argv[0], &handle))
    {
        return enif_make_badarg(env);
    }
    serialFlush(handle);
    return atom_ok;
}

static ERL_NIF_TERM
serial_put_char_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int handle, character;
    if (!enif_get_int(env, argv[0], &handle) ||
        !enif_get_int(env, argv[1], &character))
    {
        return enif_make_badarg(env);
    }
    serialPutchar(handle, (uint8_t)character);
    return atom_ok;
}

static ERL_NIF_TERM
serial_puts_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int handle, str_len;
    if (!enif_get_int(env, argv[0], &handle) ||
        !enif_get_int(env, argv[1], &str_len))
    {
        return enif_make_badarg(env);
    }
    char str[str_len+1];
    if (!enif_get_string(env, argv[2], str, sizeof(str), ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }
    serialPuts(handle, str);
    return atom_ok;
}

static ERL_NIF_TERM
serial_data_avail_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int handle;
    if (!enif_get_int(env, argv[0], &handle))
    {
        return enif_make_badarg(env);
    }
    int data_avail = serialDataAvail(handle);
    return enif_make_int(env, data_avail);
}

static ERL_NIF_TERM
serial_get_char_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int handle;
    if (!enif_get_int(env, argv[0], &handle))
    {
        return enif_make_badarg(env);
    }
    int c = serialGetchar(handle);
    return enif_make_int(env, c);
}

// SPI
static ERL_NIF_TERM
spi_get_fd_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int channel;
    if (!enif_get_int(env, argv[0], &channel))
    {
        return enif_make_badarg(env);
    }
    int fd = wiringPiSPIGetFd(channel);
    return enif_make_int(env, fd);
}

static ERL_NIF_TERM
spi_data_rw_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int channel, len;
    ErlNifBinary buf;

    if (!enif_get_int(env, argv[0], &channel) ||
	!enif_inspect_binary(env, argv[1], &buf) ||
        !enif_get_int(env, argv[2], &len))
    {
        return enif_make_badarg(env);
    }

    enif_realloc_binary(&buf, len);

    wiringPiSPIDataRW(channel, buf.data, len);
    return enif_make_binary(env, &buf);
}

static ERL_NIF_TERM
spi_setup_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int channel, speed;
    if (!enif_get_int(env, argv[0], &channel) ||
        !enif_get_int(env, argv[1], &speed))
    {
        return enif_make_badarg(env);
    }
    int result = wiringPiSPISetup(channel, speed);
    return enif_make_int(env, result);
}

static ErlNifFunc nif_funcs[] =
    {
        // the basics: pins and stuff
        {"pin_mode_nif",            2, pin_mode_nif},
        {"digital_write_nif",       2, digital_write_nif},
        {"pwm_write_nif",           2, pwm_write_nif},
        {"digital_read_nif",        1, digital_read_nif},
        {"pull_up_dn_control_nif",  2, pull_up_dn_control_nif},
        // LCD
        {"lcd_init_nif",           13, lcd_init_nif},
        {"lcd_home_nif",            1, lcd_home_nif},
        {"lcd_clear_nif",           1, lcd_clear_nif},
        {"lcd_position_nif",        3, lcd_position_nif},
        {"lcd_put_char_nif",        2, lcd_put_char_nif},
        {"lcd_puts_nif",            3, lcd_puts_nif},
        // shift
        {"shift_in_nif",            3, shift_in_nif},
        {"shift_out_nif",           4, shift_out_nif},
        // soft pwm
        {"soft_pwm_create_nif",     3, soft_pwm_create_nif},
        {"soft_pwm_write_nif",      2, soft_pwm_write_nif},
        // serial
        {"serial_open_nif",         3, serial_open_nif},
        {"serial_close_nif",        1, serial_close_nif},
        {"serial_flush_nif",        1, serial_flush_nif},
        {"serial_put_char_nif",     2, serial_put_char_nif},
        {"serial_puts_nif",         3, serial_puts_nif},
        {"serial_data_avail_nif",   1, serial_data_avail_nif},
        {"serial_get_char_nif",     1, serial_get_char_nif},
	// SPI
	{"spi_get_fd_nif",          1, spi_get_fd_nif},
	{"spi_data_rw_nif",         3, spi_data_rw_nif},
	{"spi_setup_nif",           2, spi_setup_nif}
    };

ERL_NIF_INIT(wpi, nif_funcs, load, NULL, NULL, NULL)
