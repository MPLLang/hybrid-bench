signature GEOMETRY =
sig
  structure R: REAL
  type point = R.real * R.real
  structure Point:
  sig
    val triArea: point * point * point -> R.real
  end
end
