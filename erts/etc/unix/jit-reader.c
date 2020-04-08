#include "jit-reader.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

GDB_DECLARE_GPL_COMPATIBLE_READER

typedef struct range {
    void *start;
    void *end;
} range;

typedef struct priv {
    range *ranges;
    int num_ranges;
} priv;

enum gdb_status read_debug_info(struct gdb_reader_funcs *self,
                                struct gdb_symbol_callbacks *cb,
                                void *memory, long memory_sz) {
    priv *priv = self->priv_data;
    uint64_t num_functions = *(uint64_t*)memory;
    void *mod_start  = *(void **)(memory + sizeof(uint64_t));
    void *mod_end  = mod_start + *(uint64_t*)(memory + sizeof(uint64_t)*2);
    char* module = memory + sizeof(uint64_t)*3;
    char* curr = module + strlen(module) + 1;
    int i;
    struct gdb_object *obj = cb->object_open(cb);

    priv->ranges = realloc(priv->ranges, sizeof(range) * ++priv->num_ranges);
    priv->ranges[priv->num_ranges-1].start = mod_start;
    priv->ranges[priv->num_ranges-1].end = mod_end;

//    fprintf(stderr,"Add module %s (%p, %p)\r\n", module, mod_start, mod_end);


    for (i = 0; i < num_functions; i++) {
        // get begin and end of code segment
        struct gdb_symtab *symtab = cb->symtab_open(cb, obj, module);
        GDB_CORE_ADDR begin = *(GDB_CORE_ADDR*)curr;
        GDB_CORE_ADDR end = *(GDB_CORE_ADDR*)(curr + sizeof(GDB_CORE_ADDR));
        // get name of function
        const char *name = (const char*)(curr + sizeof(GDB_CORE_ADDR) * 2);
        curr += strlen(name) + 1 + sizeof(GDB_CORE_ADDR) * 2;

//        fprintf(stderr,"Add %s:%s (%p, %p)\r\n", module, name, (void*)begin, (void*)end);

        // returned value has no use
        cb->block_open(cb, symtab, NULL, begin, end, name);
        cb->symtab_close(cb, symtab);
    }

    cb->object_close(cb, obj);
    return GDB_SUCCESS;
}

enum gdb_status unwind(struct gdb_reader_funcs *self, struct gdb_unwind_callbacks *cb){
    int i;
    priv *priv = self->priv_data;
    void *rip = *(void**)cb->reg_get(cb,16)->value;
    void *rsp = *(void**)cb->reg_get(cb,7)->value;
    for (i = 0; i < priv->num_ranges; i++) {
        if (rip >= priv->ranges[i].start && rip <= priv->ranges[i].end) {
            struct gdb_reg_value *prev_rsp = malloc(sizeof(struct gdb_reg_value) + sizeof(void*)),
                *prev_rip = malloc(sizeof(struct gdb_reg_value) + sizeof(void*)),
                *prev_rbp = malloc(sizeof(struct gdb_reg_value) + sizeof(void*));

            /* fprintf(stderr,"UNWIND match: rip: %p rsp: %p\r\n", rip, rsp); */

            prev_rsp->free = &free;
            prev_rsp->defined = 1;
            prev_rsp->size = sizeof(void*);
            ((uint64_t*)prev_rsp->value)[0] = rsp + sizeof(uint64_t) * 12 + sizeof(uint64_t) * 2;

            cb->target_read(rsp + 13 * sizeof(void*), &prev_rip->value, sizeof(void*));
            prev_rip->free = &free;
            prev_rip->defined = 1;
            prev_rip->size = sizeof(void*);

            cb->target_read(rsp + 12 * sizeof(void*), &prev_rbp->value, sizeof(void*));
            prev_rbp->free = &free;
            prev_rbp->defined = 1;
            prev_rbp->size = sizeof(void*);

            /* fprintf(stderr,"UNWIND prev: rip: %p rsp: %p rbp: %p\r\n", */
            /*         *(void**)prev_rip->value, */
            /*         *(void**)prev_rsp->value, */
            /*         *(void**)prev_rbp->value); */

            cb->reg_set(cb, 16, prev_rip);
            cb->reg_set(cb, 7, prev_rsp);
            cb->reg_set(cb, 6, prev_rbp);
            return GDB_SUCCESS;
        }
    }
    /* fprintf(stderr,"UNWIND no match: rip: %p rsp: %p\r\n", rip, rsp); */
    return GDB_FAIL;
}

struct gdb_frame_id get_frame_id(struct gdb_reader_funcs *self, struct gdb_unwind_callbacks *cb){
    int i;
    priv *priv = self->priv_data;
    struct gdb_frame_id frame = {0, 0};
    void *rip = *(void**)cb->reg_get(cb,16)->value;
    void *rsp = *(void**)cb->reg_get(cb,7)->value;
//    fprintf(stderr,"FRAME: rip: %p rsp: %p\r\n", rip, rsp);
    for (i = 0; i < priv->num_ranges; i++) {
        if (rip >= priv->ranges[i].start && rip <= priv->ranges[i].end) {
            frame.code_address = priv->ranges[i].start;
            frame.stack_address = rsp + 14 * sizeof(void*);
        }
    }
    return frame;
}

void destroy(struct gdb_reader_funcs *self){
    free(self);
}

struct gdb_reader_funcs *gdb_init_reader(void){
    struct gdb_reader_funcs *funcs = malloc(sizeof(struct gdb_reader_funcs));
    priv *priv = malloc(sizeof(priv));
    priv->num_ranges = 1;
    priv->ranges = malloc(sizeof(range));
    priv->ranges[0].start = 0;
    priv->ranges[0].end = 0;

    funcs->reader_version = GDB_READER_INTERFACE_VERSION;
    funcs->priv_data = priv;

    funcs->read = read_debug_info;
    funcs->unwind = unwind;
    funcs->get_frame_id = get_frame_id;
    funcs->destroy = destroy;

    return funcs;
}
