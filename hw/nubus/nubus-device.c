/*
 * QEMU Macintosh Nubus
 *
 * Copyright (c) 2013-2018 Laurent Vivier <laurent@vivier.eu>
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or later.
 * See the COPYING file in the top-level directory.
 *
 */

#include "qemu/osdep.h"
#include "qemu/datadir.h"
#include "exec/target_page.h"
#include "hw/irq.h"
#include "hw/loader.h"
#include "hw/nubus/nubus.h"
#include "qapi/error.h"
#include "qemu/error-report.h"


void nubus_set_irq(NubusDevice *nd, int level)
{
    NubusBus *nubus = NUBUS_BUS(qdev_get_parent_bus(DEVICE(nd)));

    qemu_set_irq(nubus->irqs[nd->slot], level);
}

static uint8_t *nubus_rom_address(DeviceState *dev,
                                  NubusDevice *nd,
                                  int64_t size)
{
    uint8_t *rom_ptr;
    int64_t align_size;

    char *name = g_strdup_printf("nubus-slot-%x-declaration-rom", nd->slot);

    /*
     * Ensure ROM memory region is aligned to target page size regardless
     * of the size of the Declaration ROM image
     */
    align_size = ROUND_UP(size, qemu_target_page_size());
    memory_region_init_rom(&nd->decl_rom, OBJECT(dev), name, align_size,
                           &error_abort);
    g_free(name);
    rom_ptr = memory_region_get_ram_ptr(&nd->decl_rom);

    memory_region_add_subregion(&nd->slot_mem, NUBUS_SLOT_SIZE - align_size,
                                &nd->decl_rom);

    return rom_ptr + (uintptr_t)(align_size - size);
}

static uint8_t *nubus_rom_lane_adjusted(DeviceState *dev,
                                        NubusDevice *nd,
                                        gchar *romdata,
                                        gsize romsize)
{
    uint8_t *rom_ptr = NULL;
    uint8_t bytelanes = romdata[romsize - 1];
    int64_t size = 0;
    int64_t i, c;
    uint8_t validate;
    int8_t lanescount = ctpop8(bytelanes & 0x0F);

    validate = (~(bytelanes & 0x0F) << 4) | (bytelanes & 0x0F);
    if (validate != bytelanes) {
        printf("bytelanes not valid: %x\n", bytelanes);
        return NULL;
    }

    size = ROUND_UP((romsize * 4) / lanescount, 4);
    rom_ptr = nubus_rom_address(dev, nd, size);
    for (i = 0, c = 0; i < romsize; i++, c += 4) {
        int setlanes = 0;
        for (int lane = 0; lane < 4; lane++) {
            if (bytelanes & (1 << lane)) {
                if ((i + setlanes) < romsize) {
                    rom_ptr[c + lane] = romdata[i + setlanes];
                    setlanes++;
                }
            }
        }
        i += (lanescount - 1);
    }

    return rom_ptr;
}

static void nubus_device_realize(DeviceState *dev, Error **errp)
{
    NubusBus *nubus = NUBUS_BUS(qdev_get_parent_bus(dev));
    NubusDevice *nd = NUBUS_DEVICE(dev);
    char *name, *path;
    hwaddr slot_offset;
    uint8_t *rom_ptr;
    gchar *romdata;
    gsize romsize;

    if (nd->slot < 0 || nd->slot >= NUBUS_SLOT_NB) {
        error_setg(errp,
                   "'slot' value %d out of range (must be between 0 and %d)",
                   nd->slot, NUBUS_SLOT_NB - 1);
        return;
    }

    /* Super */
    slot_offset = nd->slot * NUBUS_SUPER_SLOT_SIZE;

    name = g_strdup_printf("nubus-super-slot-%x", nd->slot);
    memory_region_init(&nd->super_slot_mem, OBJECT(dev), name,
                       NUBUS_SUPER_SLOT_SIZE);
    memory_region_add_subregion(&nubus->super_slot_io, slot_offset,
                                &nd->super_slot_mem);
    g_free(name);

    /* Normal */
    slot_offset = nd->slot * NUBUS_SLOT_SIZE;

    name = g_strdup_printf("nubus-slot-%x", nd->slot);
    memory_region_init(&nd->slot_mem, OBJECT(dev), name, NUBUS_SLOT_SIZE);
    memory_region_add_subregion(&nubus->slot_io, slot_offset,
                                &nd->slot_mem);
    g_free(name);

    /* Declaration ROM */
    if (nd->romfile != NULL) {
        path = qemu_find_file(QEMU_FILE_TYPE_BIOS, nd->romfile);
        if (path == NULL) {
            path = g_strdup(nd->romfile);
        }

        if (!g_file_get_contents(path, &romdata, &romsize, NULL)) {
            error_setg(errp, "failed to find romfile \"%s\"", nd->romfile);
            g_free(path);
            return;
        } else if (romsize == 0) {
            error_setg(errp, "romfile \"%s\" is empty", nd->romfile);
            g_free(path);
            return;
        } else if (romsize > NUBUS_DECL_ROM_MAX_SIZE) {
            error_setg(errp, "romfile \"%s\" too large (maximum size 128K)",
                       nd->romfile);
            g_free(path);
            return;
        }

        rom_ptr = nubus_rom_lane_adjusted(dev, nd, romdata, romsize);
        g_free(path);
        g_free(romdata);
        if (!rom_ptr) {
            error_setg(errp, "could not load romfile \"%s\"", nd->romfile);
            return;
        }
    }
}

static const Property nubus_device_properties[] = {
    DEFINE_PROP_INT32("slot", NubusDevice, slot, -1),
    DEFINE_PROP_STRING("romfile", NubusDevice, romfile),
};

static void nubus_device_class_init(ObjectClass *oc, const void *data)
{
    DeviceClass *dc = DEVICE_CLASS(oc);

    dc->realize = nubus_device_realize;
    dc->bus_type = TYPE_NUBUS_BUS;
    device_class_set_props(dc, nubus_device_properties);
}

static const TypeInfo nubus_device_type_info = {
    .name = TYPE_NUBUS_DEVICE,
    .parent = TYPE_DEVICE,
    .abstract = true,
    .instance_size = sizeof(NubusDevice),
    .class_init = nubus_device_class_init,
};

static void nubus_register_types(void)
{
    type_register_static(&nubus_device_type_info);
}

type_init(nubus_register_types)
