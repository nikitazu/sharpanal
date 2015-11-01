using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Threading.Tasks;
using System.Diagnostics;
using System.Globalization;
using System.Composition;

namespace SimpleWallet.Core.Exporters
{
    [Export]
    [Shared]
    public class QifExporter
    {
        public void Export(TextWriter writer, QifType type, IEnumerable<IExportableTransaction> exportableTransactions)
        {
            ExportToSteam(writer, type, ExportToDictionary(exportableTransactions));
        }

        public void ExportToSteam(TextWriter writer, QifType type, IEnumerable<Dictionary<string, string>> plainTransactions)
        {
            writer.Write("!Type:");
            writer.WriteLine("{0}", GetQifTypeName(type));
            foreach (var transaction in plainTransactions)
            {
                foreach (var property in transaction)
	            {
                    writer.WriteLine("{0}{1}", property.Key, property.Value);
	            }
                writer.WriteLine("^");
            }
            writer.Flush();
        }

        public IEnumerable<Dictionary<string,string>> ExportToDictionary(IEnumerable<IExportableTransaction> transactions)
        {
            return transactions.Select(t => {
                return new Dictionary<string, string>
                {
                    { "D", t.At.ToString("yyyy-MM-dd") },
                    { "T", t.Amount.HasValue ? t.Amount.Value.ToString(NumberFormatInfo.InvariantInfo) : "0.00" },
                    { "P", t.Title },
                };
            });
        }

        public string GetQifTypeName(QifType type)
        {
            return qifTypeNames[type];
        }

        private static readonly Dictionary<QifType, string> qifTypeNames = new Dictionary<QifType, string>
        {
            { QifType.Cash, "Cash" },
            { QifType.Bank, "Bank" },
            { QifType.CreditCard, "CCard" },
            { QifType.Investment, "Invst" },
            { QifType.Asset, "Oth A" },
            { QifType.Liability, "Oth L" },
            { QifType.Invoice, "Invoice" },
        };
    }
}
