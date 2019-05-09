open Utils;

[@react.component]
let make = (~onMemberInputChange) => {
  let (memberInput, setMemberInput) =
    React.useState(() => "a,1\nb,2\nc,3\nd,1\ne\nf\n");

  React.useEffect1(
    () => {
      onMemberInputChange(memberInput);
      None;
    },
    [|memberInput|],
  );

  let onChange = e => {
    let v = getEventValue(e);
    setMemberInput(_ => v);
  };

  <section className="MemberInput">
    <div className="MemberInput__input">
      <textarea
        value=memberInput
        placeholder="Enter the members of your team, one per line.\nUse a comma to assign the members to a team.\n\ni.e: toucan,team1 will assign user toucan to team1.\n\nDuplicate names will be ignored."
        className="bx--text-area MemberInput__textarea"
        onChange
      />
    </div>
  </section>;
};