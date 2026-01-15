/*
 * djMEMC, macintosh memory and interrupt controller
 * (Quadra 610/650/800 & Centris 610/650)
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#ifndef HW_MISC_DJMEMC_H
#define HW_MISC_DJMEMC_H

#include "hw/sysbus.h"

#define DJMEMC_SIZE        0x2000
#define DJMEMC_NUM_REGS    (0x38 / sizeof(uint32_t))

#define DJMEMC_MAXBANKS        10
#define DJMEMC_MAXBANK_SIZE    (64 * MiB)

struct DJMEMCDeviceClass {
    DeviceClass parent_class;

    DeviceRealize parent_realize;
};

struct DJMEMCState {
    SysBusDevice parent_obj;

    MemoryRegion mem_regs;
    MemoryRegion banks[DJMEMC_MAXBANKS];
    uint8_t banksize[DJMEMC_MAXBANKS];   /* amount of populated memory in MiB */

    /* Memory controller */
    uint32_t regs[DJMEMC_NUM_REGS];
};

#define TYPE_DJMEMC "djMEMC"
OBJECT_DECLARE_TYPE(DJMEMCState, DJMEMCDeviceClass, DJMEMC)

#endif
