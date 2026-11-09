package cleaner.storage

import cleaner.model.*
import cleaner.quarantine.QuarantineService
import java.nio.file.Path

import java.nio.charset.StandardCharsets
import java.nio.file.Files

class ReportService(config: AppConfig, history: HistoryService) {
  def save(lastScan: Option[ScanResult], quarantine: QuarantineService): Result[Path] = {
    val text = new StringBuilder("ОТЧЁТ TEMPORARY CLEANER\n")
    // ## пока поиск не запускали, в отчёте так и пишем
    if lastScan.isEmpty then text.append("Поиск ещё не запускался.\n")
    lastScan.foreach { scan =>
      text.append("Точки: " + scan.roots.mkString(", ") + "\n")
      text.append("Профили: " + scan.profiles.mkString(", ") + "\n")
      text.append("Проверено: " + scan.checked + "\nНайдено: " + scan.candidates.size + "\n")
      text.append("Предполагаемый объём: " + scan.candidates.map(_.info.size).sum + " байт\n")
      for ((category, items) <- scan.candidates.groupBy(_.rule.category).toList.sortBy(_._1)) {
        text.append(category + ": " + items.size + " объектов, " + items.map(_.info.size).sum + " байт\n")
      }
      text.append("\nНАЙДЕННЫЕ ОБЪЕКТЫ\n")
      for (c <- scan.candidates) {
        text.append(c.id.toString + " | " + c.path + " | " + c.info.size + " байт | " + c.rule.id +
          " | " + c.retentionDays + " дней | " + c.reason + "\n")
        c.warnings.foreach(w => text.append("Предупреждение: " + w + "\n"))
      }
      text.append("\nПРОПУСКИ И ОШИБКИ ДОСТУПА\n")
      scan.skipped.foreach { item =>
        // ## у пропуска теперь есть причина, здесь переводим её в понятный текст
        val why = item.reason match
          case SkipReason.DepthLimit => "Достигнута заданная глубина"
          case SkipReason.Failed(error) => error.message
        text.append(s"${item.path} — $why\n")
      }
      text.append("\nПРЕДУПРЕЖДЕНИЯ\n" + scan.warnings.mkString("\n") + "\n")
    }
    text.append("\nОПЕРАЦИИ ТЕКУЩЕГО ЗАПУСКА\n" + history.session.mkString("\n") + "\n")
    val volume = quarantine.volume.fold(_.message, bytes => s"$bytes байт по записям состояния")
    text.append(s"\nКАРАНТИН: $volume\n")
    for
      _ <- FileService.checkNoLinks(config.reportFile)
      _ <- Result.io {
        Files.createDirectories(config.reportFile.getParent)
        Files.write(config.reportFile, text.toString.getBytes(StandardCharsets.UTF_8))
      }
    yield
      history.record("REPORT", "Отчёт сохранён")
      // ## отдаём путь в меню, там покажем куда записали отчёт
      config.reportFile
  }
}
