open Utils;
open Dsl;

[@react.component]
let make = (~currentSpeaker: option(member)) => {
  let memberName =
    switch (currentSpeaker) {
    | Some({name}) => name
    | None => "No current speaker"
    };

  <section className="CurrentSpeaker">
    <h1> {memberName |> str} </h1>
  </section>;
};