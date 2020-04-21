# End Points

## EBSD handling
POST /ebsd MultipartFile EBSD
GET /ebsd [EBSD]
GET /ebsd/hash/{ebsd_hash} EBSD

## Orientation relationship handling
POST /ebsd/hash/{ebsd_hash}/orfit ORFitCfg ORFit
GET /ebsd/hash/{ebsd_hash}/orfit [ORFit]
GET /ebsd/hash/{ebsd_hash}/orfit/hash/{or_hash} ORFit

## Reconstruction handling
POST /ebsd/hash/{ebsd_hash}/orfit/hash/{or_hash}/arche ArcheCfg Arche
GET /ebsd/hash/{ebsd_hash}/orfit/hash/{or_hash}/arche [Arche]
GET /ebsd/hash/{ebsd_hash}/orfit/hash/{or_hash}/arche/hash/{arche_hash} Arche

# Types
EBSD =
    { alias : String
    , hash : String
    , createdBy : String
    }

ORFit =
    { hash : String
    , or : AxisPair
    , error : Double
    }

AxisPair =
    { axis : {x : Double, y : Double, y : Double}
    , angle : Double
    }

Arche =
    { useAvgOrientation : Bool
    , steps : Int
    }