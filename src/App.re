open Dsl;
open Utils;

let inputToMemberList = (i: string): list(member) => {
  Js_string.split("\n", i)
  |> Array.to_list
  |> List.filter(f => f != "")
  |> List.map(elem => {
       let splitByComma = Js_string.split(",", elem) |> Array.to_list;
       switch (splitByComma) {
       | [name, team] when team != "" => Some({name, team: Some(team)})
       | [name] => Some({name, team: None})
       | [name, team] when team == "" => Some({name, team: None})
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

let byTeamDraw = (pastSpeakers, members) => {
  let memberCount: float = List.length(members) |> float_of_int;
  let dueSpeakers = findDueSpeakers(pastSpeakers, members);

  // group due speakers by team
  let dueSpeakersByTeam =
    dueSpeakers
    |> List.fold_left(
         (acc, speaker) => {
           let teamName = speaker.team |> Belt.Option.getWithDefault(_, "");

           let teamList =
             teamName
             |> Js.Dict.get(acc)
             |> Belt.Option.getWithDefault(_, [])
             |> List.append([speaker]);

           Js.Dict.set(acc, teamName, teamList);

           acc;
         },
         Js.Dict.empty(),
       );

  let teams = Js.Dict.keys(dueSpeakersByTeam);

  let maxBounds: array(float) =
    teams
    |> Array.map(team =>
         team
         |> Js.Dict.get(dueSpeakersByTeam)
         |> Belt.Option.getWithDefault(_, [])
         |> List.length
         |> float_of_int
         |> (v => v /. memberCount)
       );

  let bounds =
    teams
    // transform to an immutable list
    |> Array.to_list
    // zip teams with max bounds, creating tuples
    |> List.mapi((i, team) => (team, maxBounds[i]))
    // sort list by second tuple parameter (bounds)
    |> List.sort(((_, b1), (_, b2)) => compare(b1, b2))
    // turn back to an Array (because n - 1 access is required later)
    |> Array.of_list
    // zip with index, creating tuples (value, index)
    |> Array.mapi((i, v) => (v, i))
    // accumulate the list creating an upper bound and lower bound
    |> Array.fold_left(
         (acc, ((team, bound), i)) =>
           if (i == 0) {
             Array.append(acc, [|(team, 0.0, bound)|]);
           } else {
             let (_, _, lowerBound) = acc[i - 1];
             Array.append(acc, [|(team, lowerBound, bound +. lowerBound)|]);
           },
         [||],
       )
    // convert to List because filtering is required
    |> Array.to_list
    // filter out all values where upper bound is zero (empty teams)
    |> List.filter(((_, _, upperBound)) => upperBound > 0.0);

  // get a number between [0,1]
  let rnd = Js.Math.random();

  // find the range where randomNumber lives

  // select that team
  let selectedTeamName =
    switch (
      List.find(((_, lower, upper)) => lower >= rnd && rnd <= upper, bounds)
    ) {
    | (team, _, _) =>
      Js.log(team);
      Some(team);
    | exception _ => None
    };

  Js.log((rnd, Array.of_list(bounds), selectedTeamName));

  // get the array of the team, pick a member randomly

  switch (selectedTeamName) {
  | Some(team) =>
    let remainingTeamMembers =
      team
      |> Js.Dict.get(dueSpeakersByTeam)
      |> Belt.Option.getWithDefault(_, [])
      |> Array.of_list;
    let remainingTeamsLength = Array.length(remainingTeamMembers);

    let randomIndex = Js.Math.random_int(0, remainingTeamsLength);
    Some(remainingTeamMembers[randomIndex]);
  | None => None
  };
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