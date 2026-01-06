/*
 * QEMU Brigent BootBug Nubus card
 *
 * Copyright (c) 2026 Rob Braun <bbraun@synack.net>
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#ifndef HW_NUBUS_BOOTBUG_H
#define HW_NUBUS_BOOTBUG_H

#include "hw/nubus/nubus.h"
#include "qom/object.h"
#include "hw/char/serial.h"

#define TYPE_NUBUS_BOOTBUG "nubus-bootbug"
OBJECT_DECLARE_TYPE(NubusBootBug, NubusBootBugDeviceClass,
                    NUBUS_BOOTBUG)

struct NubusBootBugDeviceClass {
    DeviceClass parent_class;

    DeviceRealize parent_realize;
};

struct NubusBootBug {
    NubusDevice parent_obj;

    MemoryRegion io;
    SerialState state;
};

#endif
