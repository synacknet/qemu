/*
 * QEMU Brigent BootBug Nubus card
 *
 * Copyright (c) 2026 Rob Braun <bbraun@synack.net>
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include "qemu/osdep.h"
#include "qapi/error.h"
#include "hw/nubus/nubus-bootbug.h"
#include "hw/char/serial.h"
#include "system/system.h"
#include "hw/qdev-properties-system.h"

/* This is address space size, so adjusted for using only one byte lane */
#define BOOTBUG_ROM_SIZE 0x40000

static MemTxResult nubus_bootbug_slot_write(void *opaque, hwaddr addr,
                                            uint64_t val, unsigned size,
                                            MemTxAttrs attrs)
{
    NubusBootBug *nbb = NUBUS_BOOTBUG(opaque);
    val &= 255;
    serial_io_ops.write(&nbb->state, (addr >> 2) % 8, val, 1);
    return MEMTX_OK;
}

static MemTxResult nubus_bootbug_slot_read(void *opaque, hwaddr addr,
                                           uint64_t *data, unsigned size,
                                           MemTxAttrs attrs)
{
    NubusBootBug *nbb = NUBUS_BOOTBUG(opaque);
    /*
     * Shift by 2 because we're only using one byte lane
     * Strip higher bits because this is mirrored across
     * the entire slot space.
     */
    *data = serial_io_ops.read(&nbb->state, (addr >> 2) % 8, 1);
    return MEMTX_OK;
}

static const MemoryRegionOps nubus_bootbug_slot_ops = {
    .read_with_attrs = nubus_bootbug_slot_read,
    .write_with_attrs = nubus_bootbug_slot_write,
    .endianness = DEVICE_BIG_ENDIAN,
    .valid = {
        .min_access_size = 1,
        .max_access_size = 4,
    },
};

static void nubus_bootbug_realize(DeviceState *dev, Error **errp)
{
    ERRP_GUARD();
    NubusBootBugDeviceClass *nbbdc = NUBUS_BOOTBUG_GET_CLASS(dev);
    NubusBootBug *nbb = NUBUS_BOOTBUG(dev);
    NubusDevice *nd = NUBUS_DEVICE(dev);
    NubusBus *nubus = NUBUS_BUS(qdev_get_parent_bus(DEVICE(nd)));
    SerialState *s = &nbb->state;

    nbbdc->parent_realize(dev, errp);
    if (*errp) {
        return;
    }

    if (!qdev_realize(DEVICE(s), NULL, errp)) {
        return;
    }

    s->irq = nubus->irqs[nd->slot];
    memory_region_init_io(&nbb->io,
                          OBJECT(nbb),
                          &nubus_bootbug_slot_ops,
                          nbb,
                          "nubus-bootbug-slot",
                          NUBUS_SLOT_SIZE - BOOTBUG_ROM_SIZE);
    memory_region_add_subregion(&nd->slot_mem, 0, &nbb->io);
}

static void nubus_bootbug_init(Object *obj)
{
    NubusBootBug *nbb = NUBUS_BOOTBUG(obj);

    object_initialize_child(obj, "serial", &nbb->state, TYPE_SERIAL);

    qdev_alias_all_properties(DEVICE(&nbb->state), obj);
}

static void nubus_bootbug_class_init(ObjectClass *oc, const void *data)
{
    DeviceClass *dc = DEVICE_CLASS(oc);
    NubusBootBugDeviceClass *nbbdc = NUBUS_BOOTBUG_CLASS(oc);

    device_class_set_parent_realize(dc, nubus_bootbug_realize,
                                    &nbbdc->parent_realize);
}

static const TypeInfo nubus_bootbug_types[] = {
    {
        .name = TYPE_NUBUS_BOOTBUG,
        .parent = TYPE_NUBUS_DEVICE,
        .instance_init = nubus_bootbug_init,
        .instance_size = sizeof(NubusBootBug),
        .class_init = nubus_bootbug_class_init,
        .class_size = sizeof(NubusBootBugDeviceClass),
    },
};

DEFINE_TYPES(nubus_bootbug_types)
