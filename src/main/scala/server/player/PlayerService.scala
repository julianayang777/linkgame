package server.player

import cats.effect.{IO, Ref}
import cats.implicits._
import linkgame.player.Player
import server.player.PlayerService.PlayerId

trait PlayerService {
  def addPlayer(name: String): IO[Either[PlayerError, (PlayerId, Player)]]
  def removePlayer(playerId: PlayerId): IO[Either[PlayerError, Unit]]
  def updatePlayer(playerId: PlayerId, name: String): IO[Either[PlayerError, Player]]
  def getPlayer(playerId: PlayerId): IO[Either[PlayerError, Player]]
  // Might want to return IO[Either[PlayerError, List[Player]]] if it uses database
  def getAllPlayers: IO[List[Player]]
  def containsPlayer(playerId: PlayerId): IO[Boolean]
}

object PlayerService {
  type PlayerId = String

  def inMemory: IO[PlayerService] = {
    for {
      playersRef <- Ref.of[IO, Map[PlayerId, Player]](Map.empty)
    } yield new PlayerService {
      override def addPlayer(name: String): IO[Either[PlayerError, (PlayerId, Player)]] = {
        val id = name
        for {
          result <- playersRef.modify { players =>
            val player = Player(name)
            if (players.contains(id)) (players, PlayerAlreadyExists.asLeft)
            else (players + (id -> player), (id, player).asRight)
          }
        } yield result
      }

      override def removePlayer(id: PlayerId): IO[Either[PlayerError, Unit]] =
        playersRef.modify { players =>
          if (players.contains(id)) (players - id, ().asRight)
          else (players, PlayerNotFound.asLeft)
        }

      override def updatePlayer(id: PlayerId, name: String): IO[Either[PlayerError, Player]] =
        playersRef.modify { players =>
          players.get(id) match {
            case Some(player) =>
              val updatedPlayer = player.copy(name = name)
              (players.updated(id, updatedPlayer), updatedPlayer.asRight)
            case None         => (players, PlayerNotFound.asLeft)
          }
        }

      override def getPlayer(id: PlayerId): IO[Either[PlayerError, Player]] =
        playersRef.get.map { players =>
          players.get(id) match {
            case Some(player) => player.asRight
            case None         => PlayerNotFound.asLeft
          }
        }

      override def getAllPlayers: IO[List[Player]] =
        playersRef.get.map(_.values.toList)

      override def containsPlayer(id: PlayerId): IO[Boolean] =
        playersRef.get.map(_.contains(id))
    }
  }
}
