type member = {
  name: string,
  team: option(string),
};

type memberStatus =
  | Present
  | Skipped;

type participation = {
  member,
  status: memberStatus,
};

type drawStrategy =
  | ByTeam
  | Random;

module MemberComparator =
  Belt.Id.MakeComparable({
    type t = member;
    let cmp = (m1: member, m2: member) => {
      compare(m1.name, m2.name);
    };
  });