import fetch from "isomorphic-unfetch";
import auth from "basic-auth";

const API_URL = process.env.API_URL || "http://localhost:8080";

export default async (req, res) => {
  // var user = auth(req);
  // if (user?.name !== "admin" || user?.pass !== "K3nji") {
  //   res.setHeader("WWW-Authenticate", 'Basic realm="private"');
  //   res.status(401);
  //   res.json({});
  //   return;
  // }

  const token = process.env.GOOGLE_CLOUD_RUN ? await getGoogleToken() : null;
  if (req.method === "GET") {
    const r = await fetch(API_URL + "/models", {
      method: "GET",
      headers: {
        "Content-Type": "application/json",
        Authorization: `Bearer ${token}`,
      },
    });
    let type = r.status == 200 ? "success" : "error";
    try {
      let message = await r.json();
      res.status(r.status).json({ type, message });
    } catch (e) {
      console.error(r.text());
      console.error(e);
      res.status(r.status).json({ type, message: "Failed to parse json" });
    }
  } else if (req.method === "POST") {
    const r = await fetch(API_URL + "/test", {
      method: "POST",
      body: JSON.stringify(req.body),
      headers: {
        "x-model": req.headers["x-model"],
        "Content-Type": "application/json",
        Authorization: `Bearer ${token}`,
      },
    });
    let type = r.status == 200 ? "success" : "error";
    let message = await r.json();
    res.status(r.status).json({ type, message });
  } else {
    res
      .status(405)
      .json({ type: "error", message: "Must be a GET or POST request" });
  }
};

async function getGoogleToken() {
  const metadataServerTokenURL =
    "http://metadata/computeMetadata/v1/instance/service-accounts/default/identity?audience=";

  const res = await fetch(metadataServerTokenURL + API_URL, {
    headers: {
      "Metadata-Flavor": "Google",
    },
  });
  if (res.status != 200) {
    throw new Error(res.text());
  }

  const token = await res.text();
  console.info("Google token:", token);

  return token;
}
