#!/usr/bin/env bash
set -e

env -i \
  DISPLAY="${DISPLAY}" \
  WAYLAND_DISPLAY="${WAYLAND_DISPLAY}" \
  XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR}" \
  DBUS_SESSION_BUS_ADDRESS="${DBUS_SESSION_BUS_ADDRESS}" \
  XAUTHORITY="${XAUTHORITY}" \
  HOME="${HOME}" \
  USER="${USER}" \
  LANG="${LANG}" \
  LC_ALL="${LC_ALL}" \
  TERM="${TERM}" \
  PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" \
  ./gui
