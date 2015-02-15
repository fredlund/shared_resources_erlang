-record(rw,
  {
    max_readers, %% constant
    max_writers, %% constant
    readers,
    writers
  }).
