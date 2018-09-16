namespace SokobanFabulous

open System
open System.Runtime.Serialization.Formatters.Binary
open System.IO
open Fabulous.Core
open Xamarin.Forms

type App () as app = 
//module Serializer =
    inherit Application ()

    let runner = 
        Game.program
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> Program.runWithDynamicView app

    // Uncomment this code to save the application state to app.Properties using Newtonsoft.Json
    // See https://fsprojects.github.io/Elmish.XamarinForms/models.html for further  instructions.
//#if APPSAVE
    let modelId = "model"
    let levelNumber = "levelNumber"

    let serialize o  =
        use stream = new MemoryStream()
        BinaryFormatter().Serialize(stream, o)
        Convert.ToBase64String(stream.ToArray())

    let deserialize str  =
        let bytes = Convert.FromBase64String(str)
        use stream = new MemoryStream(bytes)
        BinaryFormatter().Deserialize(stream) :?> Utils.Map

    override __.OnSleep() = 
        let json = serialize runner.CurrentModel
        Console.WriteLine("OnSleep: saving model into app.Properties, json = {0}", json)

        app.Properties.[modelId] <- json
        app.Properties.[levelNumber] <- runner.CurrentModel.maximumLevelReached

    override __.OnResume() = 
        Console.WriteLine "OnResume: checking for model in app.Properties"
        try 
            match app.Properties.TryGetValue modelId with
            | true, (:? string as json) -> 

                Console.WriteLine("OnResume: restoring model from app.Properties, json = {0}", json)
                let model = deserialize(json)

                Console.WriteLine("OnResume: restoring model from app.Properties, model = {0}", (sprintf "%0A" model))
                runner.SetCurrentModel (model, Cmd.none)

            | _ -> ()
        with ex -> 
            Game.program.onError("Error while restoring model found in app.Properties", ex)
            match app.Properties.TryGetValue levelNumber with
            | true, (:? int as level) -> 
                let model = Game.initModel level
                runner.SetCurrentModel (model, Cmd.none)
            | _ -> ()

    override this.OnStart() = 
        Console.WriteLine "OnStart: using same logic as OnResume()"
        this.OnResume()
//#endif

