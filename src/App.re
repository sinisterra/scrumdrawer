open Dsl;
open Utils;

let inputToMemberList = (i: string): list(member) => {
  Js_string.split("\n", i)
  |> Array.to_list
  |> List.filter(f => f != "")
  |> List.map(elem => {
       let splitByComma = Js_string.split(",", elem);
       switch (splitByComma) {
       | [|name, team|] when team != "" => Some({name, team: Some(team)})
       | [|name|] => Some({name, team: None})
       | [|name, team|] when team == "" => Some({name, team: None})
       | _ => None
       };
     })
  |> List.fold_left(
       (acc, v) =>
         switch (v) {
         | Some(m) => acc @ [m]
         | None => acc
         },
       [],
     );
};

let findDueSpeakers =
    (pastSpeakers: list(participation), members: list(member)) => {
  let pastSpeakersSet =
    Belt.Set.fromArray(
      pastSpeakers |> List.map(({member}) => member) |> Array.of_list,
      ~id=(module MemberComparator),
    );

  let membersSet =
    Belt.Set.fromArray(
      members |> Array.of_list,
      ~id=(module MemberComparator),
    );

  let difference =
    Belt.Set.diff(membersSet, pastSpeakersSet)
    |> Belt.Set.toArray
    |> Array.to_list;

  difference;
};

type pastSpeakers = list(participation);

let randomDraw = (pastSpeakers: list(participation), members: list(member)) => {
  let dueSpeakers = findDueSpeakers(pastSpeakers, members);

  // Pick a random number between (0, List.length(members))
  let dueSpeakerCount = List.length(dueSpeakers);
  let nextIndex = Js.Math.random_int(0, dueSpeakerCount);

  switch ((dueSpeakers |> Array.of_list)[nextIndex]) {
  | m => Some(m)
  | exception _ => None
  };
};

let array_filter = (fn, a) =>
  a |> Array.to_list |> List.filter(fn) |> Array.of_list;

let byTeamDraw = (pastSpeakers, members) => {
  let dueSpeakers = findDueSpeakers(pastSpeakers, members);

  // build a map with dueSpeakers where the keys are the teams and the values are lists of members belonging to such teams

  let dueSpeakersByTeam =
    dueSpeakers
    |> List.fold_left(
         (acc, member) => {
           let teamName =
             Belt.Option.getWithDefault(member.team, "__noteam__");
           let teamList = Belt.Map.String.getWithDefault(acc, teamName, []);
           Belt.Map.String.set(acc, teamName, teamList @ [member]);
         },
         Belt.Map.String.empty,
       );

  let dueSpeakerFrequencies =
    dueSpeakersByTeam
    |> Belt.Map.String.map(_, l =>
         float_of_int(List.length(l))
         /. float_of_int(List.length(dueSpeakers))
       )
    |> Belt.Map.String.toArray
    |> Array.to_list
    |> List.filter(((_, v1)) => v1 > 0.0)
    |> List.sort(((_, v1), (_, v2)) => compare(v2, v1));

  let speakerRoulette =
    dueSpeakerFrequencies
    |> List.mapi((i, v) => (v, i))
    |> List.fold_left(
         (acc, ((team, prob), i)) =>
           if (i == 0) {
             acc @ [(team, 1.0 -. prob, 1.0)];
           } else {
             let (_, lower, _) = Array.of_list(acc)[i - 1];
             acc @ [(team, Utils.clamp(0.0, 1.0, lower -. prob), lower)];
           },
         [],
       );

  let rnd = Js.Math.random();

  (
    switch (
      List.find(
        ((_, lower, upper)) => rnd >= lower && rnd <= upper,
        speakerRoulette,
      )
    ) {
    | (team, _, _) => Some(team)
    | exception _ => None
    }
  )
  |> Belt.Option.map(
       _,
       t => {
         let ts =
           Belt.Map.String.getWithDefault(dueSpeakersByTeam, t, [])
           |> Array.of_list;
         let randomIndex = Js.Math.random_int(0, ts |> Array.length);

         Some(ts[randomIndex]);
       },
     )
  |> Belt.Option.getWithDefault(_, None);
};

[@react.component]
let make = () => {
  let (members: list(member), setMemberList) = React.useState(() => []);
  let (pastSpeakers: pastSpeakers, setPastSpeakers) =
    React.useState(() => []);
  let (currentSpeaker: option(member), setCurrentSpeaker) =
    React.useState(() => None);
  let (drawStrategy: drawStrategy, _) = React.useState(() => ByTeam);

  let onSpeakerFinish = (status: memberStatus): list(participation) => {
    switch (currentSpeaker) {
    | Some(member) => pastSpeakers @ [{member, status}]
    | None => pastSpeakers
    };
  };

  let onMemberInputChange = value => {
    setMemberList(_ => inputToMemberList(value));
  };

  let callNextPerson = (status: memberStatus) => {
    let nextPastSpeakers = onSpeakerFinish(status);
    let makeDraw =
      switch (drawStrategy) {
      | ByTeam => byTeamDraw
      | Random => randomDraw
      };

    setPastSpeakers(_ => nextPastSpeakers);
    setCurrentSpeaker(_ => makeDraw(nextPastSpeakers, members));
  };

  let skipCurrentPerson = _e => {
    callNextPerson(Skipped);
  };

  let pickSomeone = _e => {
    callNextPerson(Present);
  };

  <>
    <main className="App__main">
      <aside className="App__aside">
        <MemberInput onMemberInputChange />
      </aside>
      <section className="App__sections">
        <AppBar />
        <CurrentSpeaker currentSpeaker />
        <PastSpeakers pastSpeakers />
        <div className="SpeakerButtons">
          <button className="bx--btn bx--btn--ghost" onClick=skipCurrentPerson>
            {"Skip person" |> str}
          </button>
          <button className="bx--btn bx--btn--primary" onClick=pickSomeone>
            {"Pick someone" |> str}
          </button>
        </div>
      </section>
    </main>
  </>;
};