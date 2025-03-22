#include <stdlib.h>
#define CLAY_IMPLEMENTATION
#include "clay.h"
#include "clayhelper.h"


Clay_Arena* ClayHelper_CreateArenaWithCapacityAndMemory(size_t capacity, void *memory) {
    Clay_Arena* arena_ptr = (Clay_Arena*)malloc(sizeof(Clay_Arena));
    *arena_ptr = Clay_CreateArenaWithCapacityAndMemory(capacity, memory);
    return arena_ptr;
}

void ClayHelper_SetPointerState(Clay_Vector2* position, bool isPointerDown) {
    Clay_SetPointerState(*position, isPointerDown);
}

void HandleClayErrors(Clay_ErrorData errorData) {
    ClayHelper_ErrorHandlerWrapper* clayErrorHandlerWrapper = (ClayHelper_ErrorHandlerWrapper*) errorData.userData;
    clayErrorHandlerWrapper->errorHandlerFunction(&errorData);
}

Clay_Context* ClayHelper_Initialize(Clay_Arena* arena, Clay_Dimensions* dimensions, ClayHelper_ErrorHandlerWrapper* errorHandlerWrapper) {
    return Clay_Initialize(
        *arena, 
        *dimensions, 
        (Clay_ErrorHandler) { 
            .errorHandlerFunction = HandleClayErrors, 
            .userData = errorHandlerWrapper
        }
    );
}

void ClayHelper_UpdateScrollContainers(bool enableDragScrolling, Clay_Vector2* scrollDelta, float deltaTime) {
    Clay_UpdateScrollContainers(enableDragScrolling, *scrollDelta, deltaTime);
}

void ClayHelper_SetLayoutDimensions(Clay_Dimensions* dimensions) {
    Clay_SetLayoutDimensions(*dimensions);
}

Clay_RenderCommandArray* ClayHelper_EndLayout(void) {
    Clay_RenderCommandArray* ptr = (Clay_RenderCommandArray*)malloc(sizeof(Clay_RenderCommandArray));
    *ptr = Clay_EndLayout();
    return ptr;
}

Clay_ElementId* ClayHelper_GetElementId(Clay_String* idString) {
    Clay_ElementId* ptr = (Clay_ElementId*)malloc(sizeof(Clay_ElementId));
    *ptr = Clay_GetElementId(*idString);
    return ptr;
}

Clay_ElementId* ClayHelper_GetElementIdWithIndex(Clay_String* idString, uint32_t index) {
    Clay_ElementId* ptr = (Clay_ElementId*)malloc(sizeof(Clay_ElementId));
    *ptr = Clay_GetElementIdWithIndex(*idString, index);
    return ptr;
}

Clay_ElementData* ClayHelper_GetElementData(Clay_ElementId* id) {
    Clay_ElementData* ptr = (Clay_ElementData*)malloc(sizeof(Clay_ElementData));
    *ptr = Clay_GetElementData(*id);
    return ptr;
}