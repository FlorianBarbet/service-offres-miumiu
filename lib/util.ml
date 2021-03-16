module Result = struct
  include Result

  let ( >>= ) = Result.bind

  let ( >|= ) = Result.map
end
