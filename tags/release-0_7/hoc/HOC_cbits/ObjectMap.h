void *getHaskellPart(void* objcPart);
void setHaskellPart(void* objcPart, void* haskellPart, int immortal);
void removeHaskellPart(void* objcPart, void* haskellPart);

void objectMapStatistics(unsigned *allocated, unsigned *immortal);
