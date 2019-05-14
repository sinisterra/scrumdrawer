open Dsl;
open Utils;

let rec take = (n, list) =>
  switch (list) {
  | [] => []
  | [x, ...xs] => n == 0 ? [] : [x, ...take(n - 1, xs)]
  };

let inputToMemberList = (i: string): list(member) => {
  // split by newline
  Js_string.split("\n", i)
  |> Array.to_list
  // remove all empty strings
  |> List.filter(f => f != "")
  // build our members
  |> List.map(elem =>
       switch (Js_string.split(",", elem) |> Array.to_list |> take(2)) {
       | [name] => Some({name, team: None})
       | [name, team] when team != "" => Some({name, team: Some(team)})
       | [name, team] when team == "" => Some({name, team: None})
       | _ => None
       }
     )
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
  // build a set of the members that have already participated
  let pastSpeakersSet =
    Belt.Set.fromArray(
      pastSpeakers |> List.map(({member}) => member) |> Array.of_list,
      ~id=(module MemberComparator),
    );

  // then a set of all of our members
  let membersSet =
    Belt.Set.fromArray(
      members |> Array.of_list,
      ~id=(module MemberComparator),
    );

  // get the difference of the set
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
  let dueSpeakerCount = dueSpeakers |> List.length;

  // get a random point within the list
  let nextIndex = Js.Math.random_int(0, dueSpeakerCount);

  // convert to an array, then pick by index
  switch ((dueSpeakers |> Array.of_list)[nextIndex]) {
  | m => Some(m)
  | exception _ => None
  };
};

let listLengthAsFloat = v => v |> List.length |> float_of_int;

let byTeamDraw = (pastSpeakers, members) => {
  let dueSpeakers = findDueSpeakers(pastSpeakers, members);

  // build a map with dueSpeakers where the keys are the teams and the values are lists of members belonging to such teams

  let dueSpeakersByTeam =
    dueSpeakers
    |> List.fold_left(
         (acc, member) => {
           // get the name. If it's a None, then use a placeholder string
           let teamName =
             Belt.Option.getWithDefault(member.team, "__noteam__");

           // find the team in the accumulator map. Return an empty list if it's a new key
           let teamList = Belt.Map.String.getWithDefault(acc, teamName, []);

           // concat the new member and push it to the accumulator
           Belt.Map.String.set(acc, teamName, teamList @ [member]);
         },
         Belt.Map.String.empty,
       );

  let dueSpeakerFrequencies =
    dueSpeakersByTeam
    // find the ratio
    |> Belt.Map.String.map(_, l =>
         (l |> listLengthAsFloat) /. (dueSpeakers |> listLengthAsFloat)
       )
    |> Belt.Map.String.toArray
    |> Array.to_list
    |> List.filter(((_, v1)) => v1 > 0.0)
    // v2 before v1 so the list is order high -> low
    |> List.sort(((_, v1), (_, v2)) => compare(v2, v1));

  let speakerRoulette =
    dueSpeakerFrequencies
    // zip in the index
    |> List.mapi((i, v) => (v, i))
    // PMF (?)
    |> List.fold_left(
         (acc, ((team, prob), i)) =>
           if (i == 0) {
             acc @ [(team, Utils.clamp(0.0, 1.0, 1.0 -. prob), 1.0)];
           } else {
             let (_, lower, _) = Array.of_list(acc)[i - 1];
             acc @ [(team, Utils.clamp(0.0, 1.0, lower -. prob), lower)];
           },
         [],
       );

  // get a number!
  let rnd = Js.Math.random();

  // find it in the range
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
         // get the list of due speakers associated to this team
         let ts =
           Belt.Map.String.getWithDefault(dueSpeakersByTeam, t, [])
           |> Array.of_list;

         // pick a random person in the team
         let randomIndex = Js.Math.random_int(0, ts |> Array.length);

         // get the value
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
  let (drawStrategy: drawStrategy, setDrawStrategy) =
    React.useState(() => ByTeam);

  let onSpeakerFinish = status => {
    // assign the received status to the player, add the speaker to the end
    switch (currentSpeaker) {
    | Some(member) => pastSpeakers @ [{member, status}]
    | None => pastSpeakers
    };
  };

  let onMemberInputChange = value => {
    // update the member list
    setMemberList(_ => inputToMemberList(value));
  };

  let callNextPerson = status => {
    // get the next list of speakers after applying the status to the current speaker
    let nextPastSpeakers = onSpeakerFinish(status);

    // choose a strategy to use to pick the next speaker
    let makeDraw =
      switch (drawStrategy) {
      | ByTeam => byTeamDraw
      | Random => randomDraw
      };

    // update the past speakers
    setPastSpeakers(_ => nextPastSpeakers);
    // use the updated past speakers to draw the next speaker
    setCurrentSpeaker(_ => makeDraw(nextPastSpeakers, members));
  };

  let skipCurrentPerson = _e => {
    // skip this person
    callNextPerson(Skipped);
  };

  let pickSomeone = _e => {
    // assume the person was present
    callNextPerson(Present);
  };

  let restart = _e => {
    setPastSpeakers(_ => []);
    setCurrentSpeaker(_ => None);
  };

  let finish = _e => {
    let nextPastSpeakers = onSpeakerFinish(Present);

    setPastSpeakers(_ => nextPastSpeakers);
    setCurrentSpeaker(_ => None);
  };

  let onDrawStrategyChange = (ds: drawStrategy) => setDrawStrategy(_ => ds);

  let noMoreDueSpeakers =
    0 == List.length(findDueSpeakers(pastSpeakers, members));

  <>
    <main className="App__main">
      <aside className="App__aside">
        <MemberInput onMemberInputChange />
      </aside>
      <section className="App__sections">
        <AppBar
          left={<SelectDrawStrategy drawStrategy onDrawStrategyChange />}
          right={
            <div className="AppBar__btns">
              <button className="bx--btn bx--btn--ghost" onClick=restart>
                {"Restart" |> str}
              </button>
              <button className="bx--btn bx--btn--primary" onClick=finish>
                {"Finish" |> str}
              </button>
            </div>
          }
        />
        <section className="App__speaker">
          <CurrentSpeaker
            currentSpeaker
            buttons={
              <div className="SpeakerButtons">
                <button
                  className="bx--btn bx--btn--ghost"
                  onClick=skipCurrentPerson
                  disabled=noMoreDueSpeakers>
                  {"Skip person" |> str}
                </button>
                <button
                  className="bx--btn bx--btn--primary"
                  onClick=pickSomeone
                  disabled=noMoreDueSpeakers>
                  {"Pick someone" |> str}
                </button>
              </div>
            }
          />
          <PastSpeakers
            pastSpeakers
            memberCount={<MemberCount pastSpeakers currentSpeaker />}
          />
        </section>
      </section>
    </main>
  </>;
};