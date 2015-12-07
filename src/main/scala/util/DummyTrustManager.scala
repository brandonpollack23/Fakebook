package util

import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import javax.net.ssl.X509TrustManager;


class DummyTrustManager extends X509TrustManager {

  def isClientTrusted(cert: Array[X509Certificate]) = true

  def isServerTrusted(cert: Array[X509Certificate]) = true

  def getAcceptedIssuers: Array[X509Certificate] = new Array[X509Certificate](0)

  @throws(classOf[CertificateException])
  def checkClientTrusted(arg0: Array[X509Certificate], arg1: String): Unit  = { }
  @throws(classOf[CertificateException])
  def checkServerTrusted (arg0: Array[X509Certificate], arg1: String): Unit  = { }
}
