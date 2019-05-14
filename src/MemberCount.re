open Utils;
open Dsl;
[@react.component]
let make = (~pastSpeakers: list(participation), ~currentSpeaker) => {
  let effectiveSpeakerCount =
    pastSpeakers
    |> List.filter(({status}) =>
         switch (status) {
         | Present => true
         | _ => false
         }
       )
    |> List.length;

  let plusCurrentSpeaker =
    switch (currentSpeaker) {
    | Some(_) => 1
    | None => 0
    };

  <>
    <h3>
      {"Speaker count: "
       ++ string_of_int(effectiveSpeakerCount + plusCurrentSpeaker)
       |> str}
    </h3>
  </>;
};