#pragma once
#include "clay.h"

// Haskell FFI cannot pass structs by value to C. Most of these wrappers just
// proxy args and return values through the heap.
Clay_Arena* ClayHelper_CreateArenaWithCapacityAndMemory(size_t capacity, void *memory);

void ClayHelper_SetPointerState(Clay_Vector2* position, bool isPointerDown);

typedef struct {
    void (*errorHandlerFunction)(Clay_ErrorData* errorText);
} ClayHelper_ErrorHandlerWrapper;

Clay_Context* ClayHelper_Initialize(Clay_Arena* arena, Clay_Dimensions* dimensions, ClayHelper_ErrorHandlerWrapper* errorHandlerWrapper);

void ClayHelper_UpdateScrollContainers(bool enableDragScrolling, Clay_Vector2* scrollDelta, float deltaTime);

void ClayHelper_SetLayoutDimensions(Clay_Dimensions* dimensions);

Clay_RenderCommandArray* ClayHelper_EndLayout(void);

Clay_ElementId* ClayHelper_GetElementId(Clay_String* idString);

Clay_ElementId* ClayHelper_GetElementIdWithIndex(Clay_String* idString, uint32_t index);

Clay_ElementData* ClayHelper_GetElementData(Clay_ElementId* id);

Clay_ElementId* ClayHelper_HashString(Clay_String* label, uint32_t offset, uint32_t seed);