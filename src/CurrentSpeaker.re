open Utils;
open Dsl;

[@react.component]
let make = (~currentSpeaker: option(member), ~buttons) => {
  let memberName =
    switch (currentSpeaker) {
    | Some({name}) => name
    | None => "No current speaker"
    };

  <section className="CurrentSpeaker">
    <div className="CurrentSpeaker__name">
      <h1> {memberName |> str} </h1>
    </div>
    buttons
  </section>;
};