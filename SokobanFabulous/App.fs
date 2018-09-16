namespace SokobanFabulous

open System
open System.Runtime.Serialization.Formatters.Binary
open System.IO
open Fabulous.Core
open Xamarin.Forms

type App () as app = 
    inherit Application ()

    let runner = 
        Game.program
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> Program.runWithDynamicView app

    let modelId = "model"
    let levelNumber = "levelNumber"

    let serialize o  =
        use stream = new MemoryStream()
        BinaryFormatter().Serialize(stream, o)
        Convert.ToBase64String(stream.ToArray())

    let deserialize str  =
        let bytes = Convert.FromBase64String(str)
        use stream = new MemoryStream(bytes)
        BinaryFormatter().Deserialize(stream) :?> Levels.Map

    override __.OnSleep() = 
        let base64 = serialize runner.CurrentModel
        Console.WriteLine("OnSleep: saving model into app.Properties")

        app.Properties.[modelId] <- base64
        app.Properties.[levelNumber] <- runner.CurrentModel.maximumLevelReached

    override __.OnResume() = 
        Console.WriteLine "OnResume: checking for model in app.Properties"
        try 
            match app.Properties.TryGetValue modelId with
            | true, (:? string as base64) -> 

                Console.WriteLine("OnResume: restoring model from app.Properties")
                let model = deserialize(base64)

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

