export default {
  id: "emacs-codemode-test",
  setup(ctx) {
    ctx.session.hook("context", (event) => {
      event.system.push({
        type: "text",
        text: `Emacs Code Mode integration fixture: ${process.env.OC2_EMACS_SERVICE_TEST ?? "unset"}`,
      });
    });
    ctx.tool.transform((editor) => {
      editor.add({
        name: "emacs_probe",
        description: "Return a deterministic result for the Emacs integration test.",
        input: {
          type: "object",
          properties: { value: { type: "string" } },
          required: ["value"],
          additionalProperties: false,
        },
        async execute({ value }) {
          if (value === "fail") throw new Error("probe failure");
          return { content: `probe result: ${value}` };
        },
      });
    });
  },
};
