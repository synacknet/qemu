/*
 * djMEMC, macintosh memory and interrupt controller
 * (Quadra 610/650/800 & Centris 610/650)
 *
 *    https://mac68k.info/wiki/display/mac68k/djMEMC+Information
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include "qemu/osdep.h"
#include "qemu/log.h"
#include "qemu/units.h"
#include "migration/vmstate.h"
#include "hw/misc/djmemc.h"
#include "hw/qdev-properties.h"
#include "hw/boards.h"
#include "trace.h"
#include "qapi/error.h"


#define DJMEMC_INTERLEAVECONF   0x0
#define DJMEMC_BANK0CONF        0x4
#define DJMEMC_BANK1CONF        0x8
#define DJMEMC_BANK2CONF        0xc
#define DJMEMC_BANK3CONF        0x10
#define DJMEMC_BANK4CONF        0x14
#define DJMEMC_BANK5CONF        0x18
#define DJMEMC_BANK6CONF        0x1c
#define DJMEMC_BANK7CONF        0x20
#define DJMEMC_BANK8CONF        0x24
#define DJMEMC_BANK9CONF        0x28
#define DJMEMC_MEMTOP           0x2c
#define DJMEMC_CONFIG           0x30
#define DJMEMC_REFRESH          0x34


static uint64_t djmemc_read(void *opaque, hwaddr addr, unsigned size)
{
    DJMEMCState *s = opaque;
    uint64_t val = 0;

    switch (addr) {
    case DJMEMC_INTERLEAVECONF:
    case DJMEMC_BANK0CONF ... DJMEMC_BANK9CONF:
    case DJMEMC_MEMTOP:
    case DJMEMC_CONFIG:
    case DJMEMC_REFRESH:
        val = s->regs[addr >> 2];
        break;
    default:
        qemu_log_mask(LOG_UNIMP, "djMEMC: unimplemented read addr=0x%"PRIx64
                                 " val=0x%"PRIx64 " size=%d\n",
                                 addr, val, size);
    }

    trace_djmemc_read(addr, val, size);
    return val;
}

static void djmemc_write(void *opaque, hwaddr addr, uint64_t val,
                         unsigned size)
{
    DJMEMCState *s = opaque;
    int banknum;

    trace_djmemc_write(addr, val, size);

    switch (addr) {
    case DJMEMC_BANK0CONF ... DJMEMC_BANK9CONF:
        if (s->banksize[0] > 0) {
            banknum = (int)((addr >> 2) - 1);
            memory_region_set_address(&s->banks[banknum], (val & 0xFF) << 22);

            if (val & 0x100) {
                /* With bit 8 set, the bank size is limited to 32 MiB */
                memory_region_set_size(&s->banks[banknum],
                                  MIN(s->banksize[banknum] * MiB, 32 * MiB));
            } else {
                memory_region_set_size(&s->banks[banknum],
                                  MIN(s->banksize[banknum] * MiB, 64 * MiB));
            }
        }
        s->regs[addr >> 2] = val;
        break;
    case DJMEMC_INTERLEAVECONF:
    case DJMEMC_MEMTOP:
    case DJMEMC_CONFIG:
    case DJMEMC_REFRESH:
        s->regs[addr >> 2] = val;
        break;
    default:
        qemu_log_mask(LOG_UNIMP, "djMEMC: unimplemented write addr=0x%"PRIx64
                                 " val=0x%"PRIx64 " size=%d\n",
                                 addr, val, size);
    }
}

static const MemoryRegionOps djmemc_mmio_ops = {
    .read = djmemc_read,
    .write = djmemc_write,
    .impl = {
        .min_access_size = 4,
        .max_access_size = 4,
    },
    .endianness = DEVICE_BIG_ENDIAN,
};

static void djmemc_realize(DeviceState *dev, Error **errp)
{
    ERRP_GUARD();
    DJMEMCState *s = DJMEMC(dev);
    DJMEMCDeviceClass *ddc = DJMEMC_GET_CLASS(dev);
    SysBusDevice *sbd = SYS_BUS_DEVICE(dev);
    MachineState *machine = MACHINE(qdev_get_machine());
    int i;
    uint32_t bank_ram_offset = 0;

    ddc->parent_realize(dev, errp);
    if (*errp) {
        return;
    }

    if (s->banksize[0] > 0) {
        /* assign banks */
        for (i = 0; i < DJMEMC_MAXBANKS; i++) {
            char *name = g_strdup_printf("djmemc-bank-%x", i);
            uint32_t aliassize = s->banksize[i] * MiB;

            s->regs[i + 1] = (i * DJMEMC_MAXBANK_SIZE) >> 22;

            memory_region_init_alias(&s->banks[i],
                                     OBJECT(dev),
                                     name,
                                     machine->ram,
                                     bank_ram_offset,
                                     aliassize);
            sysbus_init_mmio(sbd, &s->banks[i]);
            bank_ram_offset += aliassize;
        }
    }
}

static void djmemc_init(Object *obj)
{
    DJMEMCState *s = DJMEMC(obj);
    SysBusDevice *sbd = SYS_BUS_DEVICE(obj);

    memory_region_init_io(&s->mem_regs, obj, &djmemc_mmio_ops, s, "djMEMC",
                          DJMEMC_SIZE);
    sysbus_init_mmio(sbd, &s->mem_regs);
}

static void djmemc_reset_hold(Object *obj, ResetType type)
{
    DJMEMCState *s = DJMEMC(obj);

    memset(s->regs, 0, sizeof(s->regs));
}

static const VMStateDescription vmstate_djmemc = {
    .name = "djMEMC",
    .version_id = 1,
    .minimum_version_id = 1,
    .fields = (const VMStateField[]) {
        VMSTATE_UINT32_ARRAY(regs, DJMEMCState, DJMEMC_NUM_REGS),
        VMSTATE_END_OF_LIST()
    }
};

static const Property djmemc_properties[] = {
    DEFINE_PROP_UINT8("bank0", DJMEMCState, banksize[0], 0),
    DEFINE_PROP_UINT8("bank1", DJMEMCState, banksize[1], 0),
    DEFINE_PROP_UINT8("bank2", DJMEMCState, banksize[2], 0),
    DEFINE_PROP_UINT8("bank3", DJMEMCState, banksize[3], 0),
    DEFINE_PROP_UINT8("bank4", DJMEMCState, banksize[4], 0),
    DEFINE_PROP_UINT8("bank5", DJMEMCState, banksize[5], 0),
    DEFINE_PROP_UINT8("bank6", DJMEMCState, banksize[6], 0),
    DEFINE_PROP_UINT8("bank7", DJMEMCState, banksize[7], 0),
    DEFINE_PROP_UINT8("bank8", DJMEMCState, banksize[8], 0),
    DEFINE_PROP_UINT8("bank9", DJMEMCState, banksize[9], 0),
};

static void djmemc_class_init(ObjectClass *oc, const void *data)
{
    DeviceClass *dc = DEVICE_CLASS(oc);
    ResettableClass *rc = RESETTABLE_CLASS(oc);
    DJMEMCDeviceClass *ddc = DJMEMC_CLASS(oc);

    dc->vmsd = &vmstate_djmemc;
    rc->phases.hold = djmemc_reset_hold;

    device_class_set_parent_realize(dc, djmemc_realize,
                                    &ddc->parent_realize);
    device_class_set_props(dc, djmemc_properties);
}

static const TypeInfo djmemc_info_types[] = {
    {
        .name          = TYPE_DJMEMC,
        .parent        = TYPE_SYS_BUS_DEVICE,
        .instance_size = sizeof(DJMEMCState),
        .instance_init = djmemc_init,
        .class_init    = djmemc_class_init,
    },
};

DEFINE_TYPES(djmemc_info_types)
