create table job_spec (
  name            text not null,
  url             text not null,
  branch          text not null,
  file            text not null,
  proc            text not null,
  arguments       text not null,
  primary key (name)
);

create table build (
  id              integer primary key autoincrement not null,
  job_spec        text not null,
  drv             text not null,
  log             text,
  output          text
  -- foreign key (job_spec) references job_spec(name)
);
