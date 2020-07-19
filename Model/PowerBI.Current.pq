let
    // Function
    PowerBICurrentFunction = (userName as text) as table =>
    let
        Port = 
            List.Transform(
                Lines.FromBinary(
                    Folder.Contents("C:\Users\" & userName & "\Microsoft\Power BI Desktop Store App\AnalysisServicesWorkspaces")[Content]{0}{0}[Content]{[Name = "msmdsrv.port.txt"]}[Content]
                ),
                each Text.Select(Text.From(_), {"0".."9"})){0},

        Connect = AnalysisServices.Databases("localhost:"&Port)[Data]{0}[Data]{0}[Data]{0},

        Expand = Cube.Dimensions(Connect)
    in
        Expand,

    // Documentation
    PowerBICurrentType =
        type function (
            userName as (type text meta [
                Documentation.FieldCaption = "User name",
                Documentation.FieldDescription = "Windows User name"
                ])
        )
        as text meta
        [
            Documentation.Name = "PowerBI.Current",
            Documentation.Description = "Connects to current PBI model.",
            Documentation.LongDescription = "Connects to current PBI model. Requires <code>userName</code>.<p> Needs <b>exactly one</b> Power BI Desktop instance running.<p><i>Does not work in Power BI Service.</i>",
            Documentation.Category = "Model",
            Documentation.Author = "Spyros Mavroforos",
            Documentation.Source = "https://github.com/SpyrosMauro/powerquery",
            Documentation.Version = "1",
            Documentation.Examples = {[
                Description = "Connect to current model, Windows user: <code>""MyUserName""</code>",
                Code = "PowerBI.Current(""MyUserName"")",
                Result = ""
            ]}
        ]

in
    Value.ReplaceType(PowerBICurrentFunction, PowerBICurrentType)