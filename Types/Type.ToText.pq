let
    // Function
    typeToTextFunction = (typ as type, optional primitive as logical) as text => 
        let
            schema = Type.TableSchema(type table [a = typ]){0},
            result = if primitive??false then "type " & schema[Kind] else schema[TypeName]
        in
            result,


    // Documentation
    typeToTextType =
        type function (
            typ as ((type type) meta [
                Documentation.FieldCaption = "Type",
                Documentation.FieldDescription = "Type to get as text"
                ]),
            optional primitive as (type logical meta [
                Documentation.FieldCaption = "primitive",
                Documentation.FieldDescription = "Give primitive type"
                ])
        )
        as text meta
        [
            Documentation.Name = "Type.ToText",
            Documentation.Description = "Gives text representation of given type",
            Documentation.LongDescription = "Gives text representation of given type. Optional <code>primitive</code> mode.",
            Documentation.Category = "Type",
            Documentation.Author = "Spyros Mavroforos",
            Documentation.Source = "https://github.com/SpyrosMauro/powerquery",
            Documentation.Version = "1",
            Documentation.Examples = {[
                Description = "<code>type text</code> to text",
                Code = "Type.ToText ( type text ) ",
                Result = """Text.Type"""
            ],[
                Description = "Primitive <code>type text</code> to text",
                Code = "Type.ToText ( type text , true ) ",
                Result = """type text"""
            ]}
        ]

in 
    Value.ReplaceType(typeToTextFunction, typeToTextType)